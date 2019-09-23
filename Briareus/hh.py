#! /usr/bin/env nix-shell
# -*- mode: python; -*-
#! nix-shell -i "python3.7 -u" -p git swiProlog "python37.withPackages(pp: with pp; [ thespian setproctitle attrs requests ])"

import Briareus.AnaRep.Operations as AnaRep
import Briareus.BCGen.Operations as BCGen
import Briareus.Input.Operations as BInput
import Briareus.BuildSys.Hydra as BldSys
from requests.auth import HTTPBasicAuth
import argparse
import os
import sys
from thespian.actors import ActorSystem
import attr

@attr.s
class Params(object):
    builder_type = attr.ib(default=None)
    builder_conf = attr.ib(default=None)
    builder_url  = attr.ib(default=None)
    output = attr.ib(default=None)
    verbose = attr.ib(default=False)
    up_to = attr.ib(default=None)  # class UpTo
    cachedir = attr.ib(default=None)
    repo_auth = attr.ib(default=None)


def run_hh_gen(params, inp):
    asys = ActorSystem('multiprocTCPBase')
    if params.builder_type == 'hydra':
        builder = BldSys.HydraBuilder(params.builder_conf,
                                      builder_url=params.builder_url)
    else:
        raise RuntimeError('Unknown builder (known: %s), specified: %s' %
                           (', '.join(['hydra']), params.builder))

    inp_desc = BInput.get_input_descr_and_VCS_info(inp,
                                                   cachedir=params.cachedir,
                                                   verbose=params.verbose)
    bcgen = BCGen.BCGen(builder,
                        cachedir=params.cachedir,
                        verbose=params.verbose,
                        up_to=params.up_to,
                        actor_system=asys,
                        repo_auth=params.repo_auth)
    config_results = bcgen.generate(inp_desc)
    if params.up_to and not params.up_to.enough('builder_configs'):
        return config_results

    builder_cfgs, build_cfgs = config_results
    # builder_cfgs : string to send to the builder
    # build_cfgs : Generator.GeneratedConfigs
    if params.up_to == 'builder_configs':
        return builder_cfgs

    anarep = AnaRep.AnaRep(builder,
                           verbose=params.verbose,
                           up_to=params.up_to,
                           actor_system=asys)
    anarep.report_on(build_cfgs)
    return builder_cfgs


def run_hh_on_inpfile(inp_fname, params):
    inp_parts = os.path.split(inp_fname)
    outfname = params.output or os.path.join(os.getcwd(),
                                             os.path.splitext(inp_parts[-1])[0] + '.hhc')
    with open(inp_fname) as inpf:
        if not params.up_to or params.up_to.enough('builder_configs'):
            with open(outfname, 'w') as outf:
                if params.verbose: print('hh <',inp_fname,'>',outfname)
                outf.write(run_hh_gen(params, inpf.read()))
        else:
            if params.verbose: print('hh partial run, no output')
            run_hh_gen(params, inpf.read())


def run_hh(input_src, params):
    print('Running hh')
    print('input from:',input_src)
    if not input_src:
        inp = input('Briareus input spec? ')
        if params.output:
            with open(params.output, 'w') as outf:
                outf.write(run_hh_gen(params, inp=inp))
        else:
            sys.stdout.write(run_hh_gen(params, inp=inp))
    else:
        if os.path.exists(input_src):
            run_hh_on_inpfile(input_src, params)
        elif os.path.exists(input_src + '.hhd'):
            run_hh_on_inpfile(input_src + '.hhd', params)
        else:
            raise RuntimeError('Input specification not found (in %s): %s' %
                               (os.getcwd(), input_src))
    print('hh',('completed up to: ' + str(params.up_to)) if params.up_to else 'done')


class UpTo(object):
    """Specifies an endpoint for the processing (for debugging or
       informational purposes).  Note that this object does not encode
       a "no restrictions" value; None should be used instead of this
       object for an unrestricted execution.
    """

    # In order:
    valid_up_to = [ "facts", "raw_logic_output", "build_configs", "builder_configs",
                    "build_results" ]

    @staticmethod
    def valid():
        return ', '.join(UpTo.valid_up_to)

    def __init__(self, arg):
        if arg not in UpTo.valid_up_to:
            raise ValueError("The --up-to value must be one of: " + self.valid())
        self._up_to = arg

    def __eq__(self, o):
        # Compare this object to a string to see if the string matches
        # the internal value. This is usually the indicator for the
        # checker to exit because the termination point has been
        # reached.
        return self._up_to == o

    def __str__(self): return self._up_to

    def enough(self, point):
        """Returns true if the internal up_to value is one of the valid values
           at or after the point value; this indicates that enough
           processing has been performed and the code should simply
           proceed to exit.  If false, more processing was
           requested.
        """
        pt = UpTo.valid_up_to.index(point)  # Throws a ValueError if point is not in the valid list
        return UpTo.valid_up_to.index(self._up_to) >= pt


def main():
    parser = argparse.ArgumentParser(
        description='Run the Briareus (hundred hander) tool to generate build configurations.',
        prog='hh')
    parser.add_argument(
        '--output', '-o', default=None,
        help=('Output file for writing build configurations.'
              'The default is {inputfile}.hhc or stdout if no inputfile.'))
    parser.add_argument(
        '--cachedir', '-c', default=None,
        help='Git cache directory for cloning (default is $HOME/.gitscan-cache)')
    parser.add_argument(
        '--builder', '-b', default='hydra',
        help=('Backend builder to generate build configurations for.  Valid builders '
              'are: hydra.  The default is to use the hydra builder backend.'))
    parser.add_argument(
        '--builder-config', '-B', default=None, dest="builder_conf",
        help="Configuration file for backend builder")
    parser.add_argument(
        '--builder-url', '-U', default=None, dest="builder_url",
        help="URL of builder to obtain build results")
    parser.add_argument(
        '--verbose', '-v', action='store_true', help='Enable verbose output')
    parser.add_argument(
        '--up-to', '-u', default=None, dest="up_to", type=UpTo,
        help=('For debugging: run hh up to the designated point and stop, '
              'printing the results to stdout (ignoring the -o argument). '
              'Valid ending points: %s' % UpTo.valid()))
    parser.add_argument(
        '--auth', '-A', default=None, help='Repo authentication file')
    parser.add_argument(
        '--stop-daemon', '-S', dest="stopdaemon", action='store_true',
        help=('Stop daemon processes on exit.  Normally Briareus leaves daemon '
              'processes running that can be used on subsequent runs to perform '
              'GitHub queries (knowledge of previous results helps stay below '
              'GitHub request limits).  This flag causes those processes to be '
              'shutdown on exit (even if running from a previously issued command.'))
    parser.add_argument(
        'INPUT', default=None, nargs='?',
        help='Briareus input specification (file or URL or blank to read from stdin)')
    args = parser.parse_args()
    auth = open(args.auth,'r').read().strip() if args.auth else None
    if auth and ':' in auth:
        auth = HTTPBasicAuth(*tuple(auth.split(':')))
    params = Params(builder_type=args.builder,
                    builder_conf=args.builder_conf,
                    builder_url=args.builder_url,
                    output=args.output,
                    verbose=args.verbose,
                    up_to=args.up_to,
                    cachedir=args.cachedir,
                    repo_auth=auth)
    try:
        run_hh(args.INPUT, params)
    finally:
        if args.stopdaemon:
            ActorSystem().shutdown()

if __name__ == "__main__":
    main()
