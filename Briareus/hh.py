#! /usr/bin/env nix-shell
#! nix-shell -i "python3.7 -u" -p git swiProlog "python37.withPackages(pp: with pp; [ thespian setproctitle attrs requests ])"

import Briareus.AnaRep.Operations as AnaRep
from Briareus.AnaRep.Prior import ( get_prior_report, write_report_output )
import Briareus.BCGen.Operations as BCGen
import Briareus.Input.Operations as BInput
import Briareus.BuildSys.Hydra as BldSys
from Briareus.VCS.ManagedRepo import get_updated_file
import argparse
import os
import sys
from thespian.actors import ActorSystem
import attr

@attr.s
class InpConfig(object):
    hhd = attr.ib()  # Can be None to read from stdin
    input_url = attr.ib(default=None)
    input_path = attr.ib(default=None)
    builder_type = attr.ib(default=None)
    builder_conf = attr.ib(default=None)
    builder_url  = attr.ib(default=None)
    output_file = attr.ib(default=None)

@attr.s
class Params(object):
    verbose = attr.ib(default=False)
    up_to = attr.ib(default=None)  # class UpTo
    report_file = attr.ib(default=None)


def verbosely(params, *msgargs):
    if params.verbose:
        print(*msgargs)

# ----------------------------------------------------------------------
# Actual generation and reporting functions

@attr.s
class GenResult(object):
    actor_system = attr.ib()
    builder = attr.ib(default=None)
    inp_desc = attr.ib(default=None)
    repo_info = attr.ib(default=None)
    build_cfgs = attr.ib(default=None)
    builder_cfgs = attr.ib(default=None)


def run_hh_gen(params, inpcfg, inp):
    result = GenResult(actor_system=ActorSystem('multiprocTCPBase', logDefs=logcfg))
    if inpcfg.builder_type == 'hydra':
        result.builder = BldSys.HydraBuilder(inpcfg.builder_conf,
                                             builder_url=inpcfg.builder_url)
    else:
        raise RuntimeError('Unknown builder (known: %s), specified: %s' %
                           (', '.join(['hydra']), inpcfg.builder_type))

    result.inp_desc, result.repo_info = \
        BInput.input_desc_and_VCS_info(inp,
                                       verbose=params.verbose,
                                       actor_system=result.actor_system)
    bcgen = BCGen.BCGen(result.builder,
                        verbose=params.verbose,
                        up_to=params.up_to,
                        actor_system=result.actor_system)
    config_results = bcgen.generate(result.inp_desc, result.repo_info)
    if params.up_to and not params.up_to.enough('builder_configs'):
        return config_results

    result.builder_cfgs, result.build_cfgs = config_results
    # builder_cfgs : string to send to the builder
    # build_cfgs : Generator.GeneratedConfigs
    if params.up_to == 'builder_configs':
        return result

    return result


def run_hh_report(params, gen_result, prior_report):
    anarep = AnaRep.AnaRep(gen_result.builder,
                           verbose=params.verbose,
                           up_to=params.up_to,
                           actor_system=gen_result.actor_system)
    report = anarep.report_on(gen_result.inp_desc, gen_result.repo_info, gen_result.build_cfgs, prior_report)

    assert report[0] == 'report'
    return report[1]


# ----------------------------------------------------------------------

def run_hh_gen_with_files(inp, inpcfg, outputf, params):
    gen_result = run_hh_gen(params, inpcfg, inp)
    if outputf and (not params.up_to or params.up_to.enough('builder_configs')):
        outputf.write(gen_result.builder_cfgs)
    return gen_result


def run_hh_gen_on_inpfile(inp_fname, params, inpcfg):
    inp_parts = os.path.split(inp_fname)
    outfname = (inpcfg.output_file or
                os.path.join(os.getcwd(),
                             os.path.splitext(inp_parts[-1])[0] + '.hhc'))
    with open(inp_fname) as inpf:
        if not params.up_to or params.up_to.enough('builder_configs'):
            verbosely(params, 'hh <',inp_fname,'>',outfname)
            return atomic_write_to(
                outfname,
                lambda outf: run_hh_gen_with_files(inpf.read(), inpcfg, outf, params=params))
        verbosely(params, 'hh partial run, no output')
        return run_hh_gen_with_files(inpf.read(), inpcfg, None, params=params)


def upd_from_remote(src_url, src_path, fname, repolocs, actor_system=None):
    fpath = os.path.join(src_path, os.path.basename(fname))
    try:
        data = get_updated_file(src_url, fpath, repolocs, actor_system=actor_system)
        if data.error_code:
            raise RuntimeError('Error %s' % data.error_code)
    except Exception as ex:
        print('Warning: no remote update of %s from %s: %s'
              % (fpath, src_url, str(ex)),
              file=sys.stderr)
    else:
        if data.file_data:
            atomic_write_to(fname, lambda f: f.write(data.file_data))
        else:
            print('No contents obtained for %s @ %s'
                  % (fpath, src_url))


def run_hh_reporting_to(reportf, params, inpcfg, prior_report=None):
    if not inpcfg.hhd:
        inp = input('Briareus input spec? ')
        if inpcfg.output_file:
            with open(inpcfg.output_file, 'w') as outf:
                gen_result = run_hh_gen_with_files(inp, inpcfg, outf,
                                                   params=params,
                                                   inpcfg=inpcfg)
        else:
            gen_result = run_hh_gen_with_files(inp, inpcfg, sys.stdout,
                                               params=params,
                                               inpcfg=inpcfg)
    else:
        if inpcfg.input_url is not None and inpcfg.input_path is not None:
            asys = ActorSystem('multiprocTCPBase')
            upd_from_remote(inpcfg.input_url, inpcfg.input_path, inpcfg.hhd, [], asys)
            upd_from_remote(inpcfg.input_url, inpcfg.input_path, inpcfg.builder_conf, [], asys)
        ifile = (inpcfg.hhd if os.path.exists(inpcfg.hhd)
                 else ((inpcfg.hhd + '.hhd') if os.path.exists(inpcfg.hhd + '.hhd')
                       else None))
        if not ifile:
            raise RuntimeError('Input specification not found (in %s): %s' %
                               (os.getcwd(), inpcfg.hhd))
        gen_result = run_hh_gen_on_inpfile(ifile, params=params, inpcfg=inpcfg)

    # Generator cycle done, now do any reporting

    if params.up_to and not params.up_to.enough('report'):
        return

    report = run_hh_report(params, gen_result, prior_report)
    if reportf and (not params.up_to or params.up_to.enough('report')):
        write_report_output(reportf, report)


def atomic_write_to(outfname, gen_output):
    tryout = os.path.join(os.path.dirname(outfname),
                          '.' + os.path.basename(outfname) + '.new')
    with open(tryout, 'w') as outf:
        r = gen_output(outf)
    os.rename(tryout, outfname)
    return r


def run_hh(params, inpcfg):
    verbosely(params, 'Running hh')
    verbosely(params, 'input from:', inpcfg.hhd)
    if params.report_file and (not params.up_to or params.up_to.enough('report')):
        if os.path.exists(params.report_file):
            verbosely(params, 'Reporting updates to', params.report_file)
            prior_report = get_prior_report(params.report_file)
        else:
            verbosely(params, 'Reporting to', params.report_file)
            prior_report = None
        atomic_write_to(
            params.report_file,
            lambda rep_fd: run_hh_reporting_to(rep_fd, params,
                                               inpcfg=inpcfg,
                                               prior_report=prior_report))
    else:
        verbosely(params, 'No reporting')
        run_hh_reporting_to(None, params, inpcfg=inpcfg)
    verbosely(params,'hh',('completed up to: ' + str(params.up_to))
              if params.up_to else 'done')


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
        epilog=('The BRIAREUS_PAT environment variable can be used to supply "repo=token;..." '
                'specifications of access tokens needed for access to the specified repo.'),
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
              'are: hydra.  The default is to use the %(default)s builder backend.'))
    parser.add_argument(
        '--builder-config', '-B', default=None, dest="builder_conf",
        help="Configuration file for backend builder")
    parser.add_argument(
        '--builder-url', '-U', default=None, dest="builder_url",
        help="""URL of builder to obtain build results.  If not specified, no build
                results will be available for reporting.""")
    parser.add_argument(
        '--verbose', '-v', action='store_true', help='Enable verbose output')
    parser.add_argument(
        '--up-to', '-u', default=None, dest="up_to", type=UpTo,
        help='''For debugging: run hh up to the designated point and stop, printing
                the results to stdout (ignoring the -o argument).
                Valid ending points: %s''' % UpTo.valid())
    parser.add_argument(
        '--stop-daemon', '-S', dest="stopdaemon", action='store_true',
        help='''Stop daemon processes on exit.  Normally Briareus leaves daemon
                processes running that can be used on subsequent runs
                to perform GitHub queries (knowledge of previous
                results helps stay below GitHub request limits).  This
                flag causes those processes to be shutdown on exit
                (even if running from a previously issued command.''')
    parser.add_argument(
        '--input-url-and-path', '-I',
        help='''Specify an input URL from which the INPUT files (and
                builder-config, if specified) should be updated
                from. The value should be a "url+path" with an actual
                plus sign; the INPUT and builder-config files should
                exist in the "path" location at the "url".

                This is done atomically, so any existing files are not
                overwritten if the update fails; normal Briareus
                action continues even if this update is unsuccessful.

                This is particularly useful if the INPUT and
                builder-config files are contained in a private remote
                repository for which the BRIAREUS_PAT provides access
                tokens for updates.''')
    parser.add_argument(
        'INPUT', default=None, nargs='?',
        help='Briareus input specification (file or URL or blank to read from stdin)')
    args = parser.parse_args()
    input_url, input_path = args.input_url_and_path.split('+') if args.input_url_and_path else (None,None)
    params = Params(verbose=args.verbose,
                    up_to=args.up_to,
                    report_file=args.report)
    inpcfg = InpConfig(hhd=args.INPUT,
                       input_url=input_url,
                       input_path=input_path,
                       builder_type=args.builder,
                       builder_conf=args.builder_conf,
                       builder_url=args.builder_url,
                       output_file=args.output)
    try:
        run_hh(params, inpcfg=inpcfg)
    finally:
        if args.stopdaemon:
            ActorSystem().shutdown()

if __name__ == "__main__":
    main()
