# Briareus BCGen functionality: create Build Configurations from input
# specifications and repository information.

from Briareus import print_each
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
            print_each('BUILD CONFIGS', cfgs.cfg_build_configs)
            print_each('SUBREPOS', cfgs.cfg_subrepos)
            print_each('PULL REQUESTS', cfgs.cfg_pullreqs)
        if self._up_to and not self._up_to.enough("builder_configs"):
            return cfgs
        cfg_spec = self._bldsys.output_build_configurations(input_desc, cfgs,
                                                            repo_info=repo_info,
                                                            bldcfg_fname=bldcfg_fname)
        return cfg_spec, cfgs
