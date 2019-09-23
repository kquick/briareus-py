#! /usr/bin/env nix-shell
# -*- mode: python; -*-
#! nix-shell -i "python3.7 -u" -p git swiProlog "python37.withPackages(pp: with pp; [ thespian setproctitle attrs requests ])"

import Briareus.AnaRep.Operations as AnaRep
from Briareus.AnaRep.Prior import ( get_prior_report, write_report_output )
import Briareus.BCGen.Operations as BCGen
import Briareus.Input.Operations as BInput
import Briareus.BuildSys.Hydra as BldSys
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
    reportfile = attr.ib(default=None)
    verbose = attr.ib(default=False)
    up_to = attr.ib(default=None)  # class UpTo

def verbosely(params, *msgargs):
    if params.verbose:
        print(*msgargs)

def run_hh_gen(params, inp, prior_report):
    asys = ActorSystem('multiprocTCPBase')
    if params.builder_type == 'hydra':
        builder = BldSys.HydraBuilder(params.builder_conf,
                                      builder_url=params.builder_url)
    else:
        raise RuntimeError('Unknown builder (known: %s), specified: %s' %
                           (', '.join(['hydra']), params.builder))

    inp_desc, repo_info = BInput.input_desc_and_VCS_info(inp,
                                                         verbose=params.verbose,
                                                         actor_system=asys)
    bcgen = BCGen.BCGen(builder,
                        verbose=params.verbose,
                        up_to=params.up_to,
                        actor_system=asys)
    config_results = bcgen.generate(inp_desc, repo_info)
    if params.up_to and not params.up_to.enough('builder_configs'):
        return config_results, []

    builder_cfgs, build_cfgs = config_results
    # builder_cfgs : string to send to the builder
    # build_cfgs : Generator.GeneratedConfigs
    if params.up_to == 'builder_configs':
        return builder_cfgs, []

    anarep = AnaRep.AnaRep(builder,
                           verbose=params.verbose,
                           up_to=params.up_to,
                           actor_system=asys)
    report = anarep.report_on(inp_desc, repo_info, build_cfgs, prior_report)

    if params.up_to and not params.up_to.enough('report'):
        return builder_cfgs, []

    assert report[0] == 'report'
    return builder_cfgs, report[1]


def run_hh_with_files(inp, outputf, reportf, params, prior_report):
    cfgs, report = run_hh_gen(params, inp, prior_report=prior_report)
    if outputf and (not params.up_to or params.up_to.enough('builder_configs')):
        outputf.write(cfgs)
    if reportf and (not params.up_to or params.up_to.enough('report')):
        write_report_output(reportf, report)

def run_hh_on_inpfile(reportf, inp_fname, params, prior_report):
    inp_parts = os.path.split(inp_fname)
    outfname = params.output or os.path.join(os.getcwd(),
                                             os.path.splitext(inp_parts[-1])[0] + '.hhc')
    with open(inp_fname) as inpf:
        if not params.up_to or params.up_to.enough('builder_configs'):
            verbosely(params, 'hh <',inp_fname,'>',outfname)
            atomic_write_to(
                outfname,
                lambda outf: run_hh_with_files(inpf.read(), outf, reportf,
                                               params=params,
                                               prior_report=prior_report))
        else:
            verbosely(params, 'hh partial run, no output')
            run_hh_with_files(inpf.read(), None, reportf, params=params,
                              prior_report=prior_report)


def run_hh_reporting_to(reportf, input_src, params, prior_report):
    if not input_src:
        inp = input('Briareus input spec? ')
        if params.output:
            with open(params.output, 'w') as outf:
                run_hh_with_files(inp, outf, reportf,
                                  params=params, prior_report=prior_report)
        else:
            run_hh_with_files(inp, sys.stdout, reportf, params=params,
                              prior_report=prior_report)
    else:
        if os.path.exists(input_src):
            run_hh_on_inpfile(reportf, input_src, params=params,
                              prior_report=prior_report)
        elif os.path.exists(input_src + '.hhd'):
            run_hh_on_inpfile(reportf, input_src + '.hhd', params=params,
                              prior_report=prior_report)
        else:
            raise RuntimeError('Input specification not found (in %s): %s' %
                               (os.getcwd(), input_src))


def atomic_write_to(outfname, gen_output):
    tryout = os.path.join(os.path.dirname(outfname),
                          '.' + os.path.basename(outfname) + '.new')
    with open(tryout, 'w') as outf:
        gen_output(outf)
    os.rename(tryout, outfname)


def run_hh(input_src, params):
    verbosely(params, 'Running hh')
    verbosely(params, 'input from:', input_src)
    if params.reportfile and (not params.up_to or params.up_to.enough('report')):
        if os.path.exists(params.reportfile):
            verbosely(params, 'Reporting updates to', params.reportfile)
            prior_report = get_prior_report(params.reportfile)
        else:
            verbosely(params, 'Reporting to', params.reportfile)
            prior_report = None
        atomic_write_to(
            params.reportfile,
            lambda rep_fd: run_hh_reporting_to(rep_fd, input_src, params, prior_report))
    else:
        verbosely(params, 'No reporting')
        run_hh_reporting_to(None, input_src, params, None)
    verbosely(params,'hh',('completed up to: ' + str(params.up_to)) if params.up_to else 'done')


class UpTo(object):
    """Specifies an endpoint for the processing (for debugging or
       informational purposes).  Note that this object does not encode
       a "no restrictions" value; None should be used instead of this
       object for an unrestricted execution.
    """

    # In order:
    valid_up_to = [ "facts", "raw_logic_output", "build_configs", "builder_configs",
                    "build_results", "built_facts", "raw_built_analysis", "report" ]

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
        '--report', '-r', default=None,
        help=('Output file for writing build reports.  Also read as '
              'input for generating a report relative to previous reporting. '
              'The default is {inputfile}.hhr or stdout if no inputfile.'))
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
    params = Params(builder_type=args.builder,
                    builder_conf=args.builder_conf,
                    builder_url=args.builder_url,
                    output=args.output,
                    reportfile=args.report,
                    verbose=args.verbose,
                    up_to=args.up_to)
    try:
        run_hh(args.INPUT, params)
    finally:
        if args.stopdaemon:
            ActorSystem().shutdown()

if __name__ == "__main__":
    main()
