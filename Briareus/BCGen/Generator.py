# Core BCGen functionality to process input specification into build configurations

from Briareus.Types import logic_result_expr
from Briareus.Logic.Evaluation import DeclareFact, Fact, run_logic_analysis
from Briareus.Logic.InpFacts import get_input_facts
import attr


@attr.s(frozen=True)
class GeneratedConfigs(object):
    cfg_build_configs = attr.ib()  # This is a list of InpFacts BldConfig objects
    cfg_subrepos = attr.ib(factory=set)  # repo_info['subrepos']
    cfg_pullreqs = attr.ib(factory=set)  # repo_info['pullreqs']


class Generator(object):
    def __init__(self, actor_system=None, verbose=False):
        self._actor_system = actor_system
        self.verbose = verbose

    def generate_build_configs(self, input_descr, repo_info, up_to=None):
        """The core process of generating build_config information from an
           input description.  The up_to argument can request early
           return with the information "up-to" a specific point; this
           is primarily used for diagnostics and testing.
        """
        facts = get_input_facts(input_descr.RL,
                                input_descr.BL,
                                input_descr.VAR,
                                repo_info)
        if self.verbose or up_to == 'facts':
            print('%% FACTS:')
            for f in facts:
                print(str(f))
        if up_to == "facts":
            return (up_to, facts)
        r = run_logic_analysis('build_config', facts,
                               actor_system=self._actor_system,
                               verbose=self.verbose)
        if self.verbose or up_to == 'raw_logic_output':
            print('## RAW_LOGIC_OUTPUT:')
            print(str(r))
        if up_to == "raw_logic_output":
            return (up_to, r)
        if not r:
            return ([], [])
        return ("build_configs",
                GeneratedConfigs(eval(r, globals(), logic_result_expr),
                                 repo_info['subrepos'],
                                 repo_info['pullreqs']))
