# Briareus Analysis and Reporting functionality.  Extends build
# configurations with build results (as available) and generates
# output reports.

from Briareus.Types import BuildResult, logic_result_expr
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

    def report_on(self, input_desc, repo_info, build_cfgs):
        # input_desc is 'first' returned from input_desc_and_VCS_info
        # repo_info is 'second' returned from input_desc_and_VCS_info
        # build_cfgs is BCGen.Generator.GeneratedConfigs
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
        built_facts = self.built_facts(build_results)
        facts = input_facts + built_facts

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
                eval(r, globals(), logic_result_expr) if r else [])


    def get_build_results(self, build_cfgs):

        return [ BuildResult(build,
                             self._bldsys.get_build_result(build))
                 for build in build_cfgs.cfg_build_configs ]

    def built_facts(self, build_results):
        return (
            [ DeclareFact('bldres/10'),
            ] +
            [ Fact(self.built_fact(r)) for r in build_results ])

    def built_fact(self, result):
        vars = [ 'var("{v.varname}", "{v.varvalue}")'.format(v=v)
                 for v in result.bldconfig.bldvars ]
        return ('bldres({r.bldconfig.branchtype}'
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
