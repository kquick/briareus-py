# Briareus Analysis and Reporting functionality.  Extends build
# configurations with build results (as available) and generates
# output reports.

from Briareus.BCGen.BuildConfigs import BuildResult
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

    def report_on(self, build_cfgs):   # build_cfgs is BCGen.Generator.GeneratedConfigs
        print('AnaRep.report_on %d configs (%d subrepos, %d pullreqs): TBD'
              % (len(build_cfgs.cfg_build_configs),
                 len(build_cfgs.cfg_subrepos),
                 len(build_cfgs.cfg_pullreqs)))

        build_results = self.get_build_results(build_cfgs)

        if self.verbose:
            print('## CORRELATED BUILD RESULTS:')
            for each in build_results:
                print('**',each)

        built_facts = self.built_facts(build_results)

        if self.verbose:
            print('## BUILT FACTS:')
            for each in built_facts:
                print(str(each))


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
                ', {r.bldconfig.strategy}'
                ', {vars}'
                ', "{r.results.buildname}"'
                ', {r.results.nrtotal}'
                ', {r.results.nrsucceeded}'
                ', {r.results.nrfailed}'
                ', {r.results.nrscheduled}'
                ', {r.results.cfgerror}'
                ')'
        ).format(r=result, vars='[ ' + ', '.join(vars) + ' ]')
