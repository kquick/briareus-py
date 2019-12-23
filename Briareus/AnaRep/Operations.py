"""Briareus Analysis and Reporting functionality.  Extends build
   configurations with build results (as available) and generates
   output reports.

   The AnaRep functionality uses the Logic analysis to process the
   build configurations and the collected build results to create a
   report, an analysis, and recommended actions.  The logic analysis
   is performed via multiple layers of logic:

     - actions
     - analysis
     - report

   The higher-level logic layers can (and should) use the lower-layer
   results.

"""

import attr
import functools
from Briareus.Types import BuildResult, logic_result_expr, ProjectSummary
from Briareus.Logic.InpFacts import get_input_facts
from Briareus.Logic.Evaluation import DeclareFact, Fact, run_logic_analysis


@attr.s
class ResultSet(object):

    # At present, the builder is used by reporting to get the build
    # results, so any builder will suffice.  In the future, all
    # builders should be the same, and the first generated should be
    # threaded through, or the builder map should be passed to AnaRep
    # for consuting project-specific builders for results.
    builder = attr.ib(default=None)

    # inp_desc is an array of Briareus.Input.Description.InputDesc
    # object.  These are kept separate so that the set of input logic
    # facts are consistent to each inp_desc.
    inp_desc = attr.ib(default=None)

    # repo_info is a dictionary gathered from the remote repositories.
    # Assume that the dictionaries can simply be combined and that any
    # duplicates are identical.  This might not be true if the same
    # name were chosen for different repositories, but we currently
    # deem that a bad input configuration.
    repo_info = attr.ib(default=None)

    # build_cfgs is the internal build configs,
    # Briareus.BCGen.Generator.GeneratedConfigs
    build_cfgs = attr.ib(default=None)


class AnaRep(object):
    def __init__(self,
                 actor_system=None,
                 verbose=False,
                 up_to=None):
        self._actor_system = actor_system
        self.verbose = verbose
        self._up_to = up_to  # None or UpTo

    def report_on(self, result_sets, prior_report):
        # result_sets is an array of ResultSet from hh.py, each containing:
        #   builder is the builder for the results
        #   input_descs is an array of the 'first' returned from input_desc_and_VCS_info
        #   repo_info is 'second' returned from input_desc_and_VCS_info
        #   build_cfgs is BCGen.Generator.GeneratedConfigs


        summary = ProjectSummary(
            project_name='+'.join([r.repo_name
                                   for e in result_sets
                                   for r in e.inp_desc.RL
                                   if r.project_repo]),
            bldcfg_count = sum([len(e.build_cfgs.cfg_build_configs) for e in result_sets]),
            subrepo_count = sum([len(e.build_cfgs.cfg_subrepos) for e in result_sets]),
            pullreq_count = sum([len(e.build_cfgs.cfg_pullreqs) for e in result_sets]))

        if self.verbose:
            print('## AnaRep.report_on %d configs (%d subrepos, %d pullreqs)'
                  % (summary.bldcfg_count, summary.subrepo_count, summary.pullreq_count))

        build_results = functools.reduce(
            lambda bres, e: bres + self.get_build_results(e.builder, e.build_cfgs),
            result_sets, [])

        if self.verbose:
            print('## CORRELATED BUILD RESULTS:')
            for each in build_results:
                print('**',each)

        declared_facts = [
            # ----------------------------------------------------------------------
            # Facts used for analysis and reporting

            DeclareFact('email_domain_whitelist/1'),
            DeclareFact('email_domain_blacklist/1'),
            DeclareFact('email_user_blacklist/1'),

            DeclareFact('action_type/2'),
            DeclareFact('action_type/4'),
            DeclareFact('project_owner/2'),

        ]

        input_facts = functools.reduce(
            lambda facts, e: facts.union(get_input_facts(e.inp_desc.RL,
                                                         e.inp_desc.BL,
                                                         e.inp_desc.VAR,
                                                         e.repo_info)),
            result_sets, set())

        prior_facts = mk_prior_facts(prior_report)
        built_facts = mk_built_facts(build_results)
        facts = (declared_facts +
                 sorted(list(input_facts), key=str) +
                 sorted(list(prior_facts), key=str) +
                 sorted(list(built_facts), key=str))
        raw = '\n'.join([each.inp_desc.REP.get('logic', '') for each in result_sets])

        if self.verbose or self._up_to == 'built_facts':
            print('## BUILT FACTS:')
            for each in facts:
                print(str(each))
            print(raw)
        if self._up_to == 'built_facts':
            return (up_to, facts)

        r = run_logic_analysis('built_analysis', facts,
                               raw_logic=raw,
                               actor_system=self._actor_system,
                               verbose=self.verbose)
        if self.verbose or self._up_to == 'raw_built_analysis':
            print('## RAW BUILT ANALYSIS:')
            print(str(r))
        if self._up_to == 'raw_built_analysis':
            return (self._up_to, r)

        return ("report",
                [summary] +
                (eval(r, globals(), logic_result_expr) if r else []))


    def get_build_results(self, buildsys, build_cfgs):
        return [ BuildResult(build, buildsys.get_build_result(build))
                 for build in build_cfgs.cfg_build_configs ]

def mk_prior_facts(prior_report):
    return set(
        [ DeclareFact('prior_status/7'),
          DeclareFact('prior_summary/4'),
        ] +
        list(filter(None, [ prior_fact(p) for p in (prior_report or []) ])))

def mk_built_facts(build_results):
    return set(
        [ DeclareFact('bldres/11'),
          DeclareFact('report/1'),
          DeclareFact('status_report/7'),
          DeclareFact('succeeded/0'),
          DeclareFact('failed/0'),
          DeclareFact('initial_success/0'),
          DeclareFact('fixed/0'),
        ] +
        list(filter(None, [ built_fact(r) for r in build_results ])))

def prior_fact(prior):
    return { 'ProjectSummary': prior_fact_ProjectSummary,
             'StatusReport': prior_fact_StatusReport,
             'VarFailure': prior_fact_VarFailure,
             'CompletelyFailing': lambda p: None,  # ignored as a prior
             'ConfigError': lambda p: None, # ignored as a prior
             'PR_Success': lambda p: None, # ignored as a prior
             'PR_Failure': lambda p: None, # ignored as a prior
             'PR_Failing': lambda p: None, # ignored as a prior  # KWQ: old
             'SepHandledVar': lambda p: None, # ignored as a prior
             'MergeablePR': lambda p: None, # ignored as a prior
             'Notify': lambda p: None, # ignored as a prior
             'SendEmail': prior_fact_SendEmail, # ignored as a prior
             'PostChatMessage': lambda p: None, # ignored as a prior
    }[prior.__class__.__name__](prior)

def prior_fact_ProjectSummary(prior):
    return Fact(('prior_summary("{p.project_name}"'
                 ', {p.bldcfg_count}'
                 ', {p.subrepo_count}'
                 ', {p.pullreq_count}'
                 ')'
    ).format(p=prior))

def prior_fact_StatusReport(prior):
    vars = [ 'varvalue("{v.projrepo}", "{v.varname}", "{v.varvalue}")'.format(v=v)
             for v in prior.bldvars ]
    return Fact(
        ('prior_status({p.status}'
         ', project("{p.project}")'
         ', {strategy}'
         ', {p.branchtype}'
         ', "{p.branch}"'
         ', "{p.buildname}"'
         ', {vars}'
         ')'
         ).format(p=prior, strategy=prior.strategy.lower(), vars='[ ' + ', '.join(vars) + ' ]'))

def prior_fact_VarFailure(prior):
    return None

def prior_fact_SendEmail(prior):
    targets = [ '"%s"' % A for A in prior.recipients ]
    sent = [ '"%s"' % A for A in prior.sent_to ]
    return Fact(('email('
                 '[ {send_to} ]'
                 ', notify({prior.notification.what}'
                 '  , "{prior.notification.item}"'
                 '  , varvalue('
                 '      "{prior.notification.params.projrepo}"'
                 '    , "{prior.notification.params.varname}"'
                 '    , "{prior.notification.params.varvalue}"'
                 '  )'
                 ')'
                 ', [ {sent_to} ]'
                 ')'
                 ).format(prior=prior,
                          send_to = ', '.join(targets),
                          sent_to = ', '.join(sent),
                 ))


def built_fact(result):
    vars = [ 'varvalue("{r.bldconfig.projectname}", "{v.varname}", "{v.varvalue}")'.format(v=v, r=result)
             for v in result.bldconfig.bldvars ]
    if isinstance(result.results, str):
        # Builder returned a failure / warning message and not an
        # actual list of results.
        return None
    return Fact(
        ('bldres("{r.bldconfig.projectname}"'
         ', {r.bldconfig.branchtype}'
         ', "{r.bldconfig.branchname}"'
         ', {strategy}'
         ', {vars}'
         ', "{r.results.buildname}"'
         ', {r.results.nrtotal}'
         ', {r.results.nrsucceeded}'
         ', {r.results.nrfailed}'
         ', {r.results.nrscheduled}'
         ', {configStatus}'
         ')'
        ).format(r=result,
                 vars='[ ' + ', '.join(vars) + ' ]',
                 strategy=result.bldconfig.strategy.lower(),
                 configStatus=('configError'
                               if result.results.cfgerror else 'configValid')
        )
    )
