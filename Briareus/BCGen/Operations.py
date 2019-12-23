# Briareus BCGen functionality: create Build Configurations from input
# specifications and repository information.

import Briareus.Input.Parser as Parser
import Briareus.BCGen.Generator as Generator

class BCGen(object):
    def __init__(self, bldsys, actor_system=None, verbose=False, up_to=None):
        self._bldsys = bldsys
        self._actor_system = actor_system
        self.verbose = verbose
        self._up_to = up_to  # None or UpTo

    def generate(self, input_desc, repo_info, bldcfg_fname=None):
        gen = Generator.Generator(actor_system=self._actor_system,
                                  verbose=self.verbose)
        (rtype, cfgs) = gen.generate_build_configs(input_desc, repo_info,
                                                   up_to=self._up_to)
        # cfgs : Generator.GeneratedConfigs
        if rtype != "build_configs":   # early up_to abort
            return cfgs
        if self.verbose or self._up_to == "build_configs":
            print('## BUILD CONFIGS:')
            for each in cfgs.cfg_build_configs:
                print(str(each))
            print('## SUBREPOS:')
            for each in cfgs.cfg_subrepos:
                print(str(each))
            print('## PULL REQUESTS:')
            for each in cfgs.cfg_pullreqs:
                print(str(each))
        if self._up_to and not self._up_to.enough("builder_configs"):
            return cfgs
        cfg_spec = self._bldsys.output_build_configurations(input_desc, cfgs,
                                                            repo_info=repo_info,
                                                            bldcfg_fname=bldcfg_fname)
        return cfg_spec, cfgs
