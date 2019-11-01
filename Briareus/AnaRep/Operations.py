# Briareus Analysis and Reporting functionality.  Extends build
# configurations with build results (as available) and generates
# output reports.

from Briareus.Types import BuildResult, logic_result_expr, ProjectSummary
from Briareus.Logic.InpFacts import get_input_facts
from Briareus.Logic.Evaluation import DeclareFact, Fact, run_logic_analysis


class AnaRep(object):
    def __init__(self, bldsys,
                 actor_system=None,
                 verbose=False,
                 up_to=None):
        self._bldsys = bldsys
        self._actor_system = actor_system
        self.verbose = verbose
        self._up_to = up_to  # None or UpTo

    def report_on(self, input_desc, repo_info, build_cfgs, prior_report=[]):
        # input_desc is 'first' returned from input_desc_and_VCS_info
        # repo_info is 'second' returned from input_desc_and_VCS_info
        # build_cfgs is BCGen.Generator.GeneratedConfigs

        project_name = [r.repo_name for r in input_desc.RL if r.project_repo][0]

        if self.verbose:
            print('## AnaRep.report_on %d configs (%d subrepos, %d pullreqs)'
                  % (len(build_cfgs.cfg_build_configs),
                     len(build_cfgs.cfg_subrepos),
                     len(build_cfgs.cfg_pullreqs)))

        build_results = self.get_build_results(build_cfgs)

        if self.verbose:
            print('## CORRELATED BUILD RESULTS:')
            for each in build_results:
                print('**',each)

        input_facts = get_input_facts(input_desc.RL,
                                      input_desc.BL,
                                      input_desc.VAR,
                                      repo_info)
        prior_facts = mk_prior_facts(prior_report)
        built_facts = mk_built_facts(build_results)
        facts = input_facts + prior_facts + built_facts

        if self.verbose or self._up_to == 'built_facts':
            print('## BUILT FACTS:')
            for each in facts:
                print(str(each))
        if self._up_to == 'built_facts':
            return (up_to, facts)

        r = run_logic_analysis('built_analysis', facts,
                               actor_system=self._actor_system,
                               verbose=self.verbose)
        if self.verbose or self._up_to == 'raw_built_analysis':
            print('## RAW BUILT ANALYSIS:')
            print(str(r))
        if self._up_to == 'raw_built_analysis':
            return (self._up_to, r)

        return ("report",
                [ProjectSummary(project_name=project_name,
                                bldcfg_count=len(build_cfgs.cfg_build_configs),
                                subrepo_count=len(build_cfgs.cfg_subrepos),
                                pullreq_count=len(build_cfgs.cfg_pullreqs))] +
                (eval(r, globals(), logic_result_expr) if r else []))


    def get_build_results(self, build_cfgs):

        return [ BuildResult(build,
                             self._bldsys.get_build_result(build))
                 for build in build_cfgs.cfg_build_configs ]

def mk_prior_facts(prior_report):
    return (
        [ DeclareFact('prior_status/5'),
          DeclareFact('prior_summary/4'),
        ] +
        list(filter(None, [ prior_fact(p) for p in (prior_report or []) ])))

def mk_built_facts(build_results):
    return (
        [ DeclareFact('bldres/10'),
          DeclareFact('report/1'),
          DeclareFact('status_report/5'),
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
         ', "{p.buildname}"'
         ', {vars}'
         ')'
         ).format(p=prior, strategy=prior.strategy.lower(), vars='[ ' + ', '.join(vars) + ' ]'))

def prior_fact_VarFailure(prior):
    return None

def built_fact(result):
    vars = [ 'varvalue("{r.bldconfig.projectname}", "{v.varname}", "{v.varvalue}")'.format(v=v, r=result)
             for v in result.bldconfig.bldvars ]
    if isinstance(result.results, str):
        # Builder returned a failure / warning message and not an
        # actual list of results.
        return None
    return Fact(
        ('bldres({r.bldconfig.branchtype}'
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
