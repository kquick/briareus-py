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
class Params(object):
    builder_type = attr.ib(default=None)
    builder_conf = attr.ib(default=None)
    builder_url  = attr.ib(default=None)
    output = attr.ib(default=None)
    reportfile = attr.ib(default=None)
    verbose = attr.ib(default=False)
    up_to = attr.ib(default=None)  # class UpTo
    input_url = attr.ib(default=None)
    input_path = attr.ib(default=None)


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


def upd_file_atomically(srcf, data):
    with open(srcf + '_upd', 'w') as updf:
        updf.write(data)
    os.rename(srcf + '_upd', srcf)

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
            upd_file_atomically(fname, data.file_data)
        else:
            print('No contents obtained for %s @ %s'
                  % (fpath, src_url))


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
        if params.input_url is not None and params.input_path is not None:
            asys = ActorSystem('multiprocTCPBase', logDefs=logcfg)
            upd_from_remote(params.input_url, params.input_path, input_src, [], asys)
            upd_from_remote(params.input_url, params.input_path, params.builder_conf, [], asys)
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
    params = Params(builder_type=args.builder,
                    builder_conf=args.builder_conf,
                    builder_url=args.builder_url,
                    input_url=input_url,
                    input_path=input_path,
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
