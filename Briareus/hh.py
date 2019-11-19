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
import os.path
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

    def fixup(self):
        expand_filerefs = lambda v: os.path.normpath(os.path.expanduser(os.path.expandvars(v)))
        self.hhd = expand_filerefs(self.hhd)
        self.builder_conf = expand_filerefs(self.builder_conf)
        self.output_file = expand_filerefs(self.output_file)


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
    result_sets = attr.ib(factory=list) # list of AnaRep.ResultSet

    def add_results(self, builder, inp_desc, repo_info, build_cfgs):
        self.result_sets.append(AnaRep.ResultSet(builder, inp_desc, repo_info, build_cfgs))


def run_hh_gen(params, inpcfg, inp, prev_gen_result=None):
    verbosely(params, 'Generating Build Configurations from %s' % inpcfg.hhd)
    result = (prev_gen_result or
              GenResult(actor_system=ActorSystem('multiprocTCPBase')))
    if inpcfg.builder_type == 'hydra':
        builder = BldSys.HydraBuilder(inpcfg.builder_conf,
                                      builder_url=inpcfg.builder_url)
    else:
        raise RuntimeError('Unknown builder (known: %s), specified: %s' %
                           (', '.join(['hydra']), inpcfg.builder_type))

    inp_desc, repo_info = \
        BInput.input_desc_and_VCS_info(inp,
                                       verbose=params.verbose,
                                       actor_system=result.actor_system)

    bcgen = BCGen.BCGen(builder,
                        verbose=params.verbose,
                        up_to=params.up_to,
                        actor_system=result.actor_system)
    config_results = bcgen.generate(inp_desc, repo_info)
    if params.up_to and not params.up_to.enough('builder_configs'):
        return config_results

    builder_cfgs, build_cfgs = config_results
    # builder_cfgs : string to send to the builder
    # build_cfgs : Generator.GeneratedConfigs

    result.add_results(builder, inp_desc, repo_info, build_cfgs)

    if params.up_to == 'builder_configs':
        return result

    return result, builder_cfgs


def run_hh_report(params, gen_result, prior_report):
    verbosely(params, 'Generating Analysis/Report')
    anarep = AnaRep.AnaRep(verbose=params.verbose,
                           up_to=params.up_to,
                           actor_system=gen_result.actor_system)
    report = anarep.report_on(gen_result.result_sets, prior_report)

    assert report[0] == 'report'
    return report[1]


# ----------------------------------------------------------------------

def run_hh_gen_with_files(inp, inpcfg, outputf, params, prev_gen_result=None):
    r = run_hh_gen(params, inpcfg, inp, prev_gen_result=prev_gen_result)
    if r is None:
        # Probably an --up-to prevented the full generation
        return None
    gen_result, builder_cfgs = r
    if outputf and (not params.up_to or params.up_to.enough('builder_configs')):
        outputf.write(builder_cfgs)
    return gen_result


def run_hh_gen_on_inpfile(inp_fname, params, inpcfg, prev_gen_result=None):
    inp_parts = os.path.split(inp_fname)
    outfname = (inpcfg.output_file or
                os.path.join(os.getcwd(),
                             os.path.splitext(inp_parts[-1])[0] + '.hhc'))
    with open(inp_fname) as inpf:
        if not params.up_to or params.up_to.enough('builder_configs'):
            verbosely(params, 'hh <',inp_fname,'>',outfname)
            return atomic_write_to(
                outfname,
                lambda outf: run_hh_gen_with_files(inpf.read(), inpcfg, outf,
                                                   params=params,
                                                   prev_gen_result=prev_gen_result))
        verbosely(params, 'hh partial run, no output')
        return run_hh_gen_with_files(inpf.read(), inpcfg, None,
                                     params=params,
                                     prev_gen_result=prev_gen_result)


def upd_from_remote(src_url, src_path, fname, repolocs, actor_system=None):
    fpath = os.path.join(src_path, os.path.basename(fname))
    try:
        data = get_updated_file(src_url, fpath, repolocs, 'master', actor_system=actor_system)
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


def run_hh_on_inpcfg(inpcfg, params, prev_gen_result=None):
    if inpcfg.input_url is not None and inpcfg.input_path is not None:
        asys = ((prev_gen_result.actor_system if prev_gen_result else None)
                or ActorSystem('multiprocTCPBase'))
        upd_from_remote(inpcfg.input_url, inpcfg.input_path, inpcfg.hhd, [], asys)
        upd_from_remote(inpcfg.input_url, inpcfg.input_path, inpcfg.builder_conf, [], asys)
    ifile = (inpcfg.hhd if os.path.exists(inpcfg.hhd)
                 else ((inpcfg.hhd + '.hhd') if os.path.exists(inpcfg.hhd + '.hhd')
                       else None))
    if not ifile:
        raise RuntimeError('Input specification not found (in %s): %s' %
                           (os.getcwd(), inpcfg.hhd))
    return run_hh_gen_on_inpfile(ifile, params=params, inpcfg=inpcfg, prev_gen_result=prev_gen_result)


def read_inpcfgs_from(inputArg):
    if inputArg is None:
        inp = input('Briareus input configurations? ')
    else:
        with open(inputArg) as inpf:
            inp = inpf.read()
    # A parser we already have, although it's dangerous...
    inpConfigs = eval(inp)
    for each in inpConfigs:
        each.fixup()
    return inpConfigs


def run_hh_reporting_to(reportf, params, inputArg=None, inpcfg=None, prior_report=None):
    """Runs the Briareus operation, writing the output to reportf if not
       None. If inpcfg is set, then this is for that single
       configuration, otherwise the input configurations are read from
       inputArg (stdin if inputArg is None).

    """
    if inpcfg is None:
        inpcfgs = read_inpcfgs_from(inputArg)
        if not inpcfgs:
            raise ValueError('No input configurations specified')
        gen_result = None
        for inpcfg in inpcfgs:
            gen_result = run_hh_on_inpcfg(inpcfg, params, prev_gen_result=gen_result)

    else:
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
            gen_result = run_hh_on_inpcfg(inpcfg, params)

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


def run_hh(params, inpcfg=None, inputArg=None):
    verbosely(params, 'Running hh')
    if inpcfg is None:
        verbosely(params, 'multiple input configs from:', inputArg or 'stdin')
    else:
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
                                               inputArg=inputArg,
                                               inpcfg=inpcfg,
                                               prior_report=prior_report))
    else:
        verbosely(params, 'No reporting')
        run_hh_reporting_to(None, params, inputArg=inputArg, inpcfg=inpcfg)
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
        '--cfg-input', '-C', dest='cfginput', action='store_true',
        help='''Input file specifies a python list of InpConfig values describing
                multiple projects to process.  This flag is
                incompatible with the -U, -B, -b, -I, and -o
                flags, and changes the interpretation of the input
                file from a single Briareus input specification to a
                list of InpConfig structures, each describing a
                Briareus project.  This mode is used when the Analysis
                and Reporting (AnaRep) phase should consider the
                results of all projects instead of just a single
                project.''')
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
    params = Params(verbose=args.verbose,
                    up_to=args.up_to,
                    report_file=args.report)
    if args.cfginput:
        if args.builder_url or args.builder_conf or \
           args.input_url_and_path or args.output:
            raise ValueError('Cannot use -C with any of: -U, -B, -b, -I, or -o')
        inpcfg = None
        inputArg = args.INPUT
    else:
        input_url, input_path = args.input_url_and_path.split('+') if args.input_url_and_path else (None,None)
        inpcfg = InpConfig(hhd=args.INPUT,
                           input_url=input_url,
                           input_path=input_path,
                           builder_type=args.builder,
                           builder_conf=args.builder_conf,
                           builder_url=args.builder_url,
                           output_file=args.output)
        inputArg = None
    try:
        run_hh(params, inpcfg=inpcfg, inputArg=inputArg)
    finally:
        if args.stopdaemon:
            ActorSystem().shutdown()

if __name__ == "__main__":
    main()
