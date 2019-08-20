# Briareus Analysis and Reporting functionality.  Extends build
# configurations with build results (as available) and generates
# output reports.

import attr

@attr.s(frozen=True)
class BuildResult(object):
    bldconfig = attr.ib()  # BCGen.BuildConfigs.BldConfig
    results = attr.ib()  # ??


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

        for each in build_results:
            print('**',each)


    def get_build_results(self, build_cfgs):

        return [ BuildResult(build,
                             self._bldsys.get_build_result(build))
                 for build in build_cfgs.cfg_build_configs ]
