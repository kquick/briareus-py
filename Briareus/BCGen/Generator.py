# Core BCGen functionality to process input specification into build configurations

from Briareus import print_titled, print_each
from Briareus.Types import logic_result_expr, BldConfig, UpTo
from Briareus.VCS_API import PRInfo, RepoInfoTy, RepoSite
from Briareus.Input.Description import InputDesc, RepoDesc
from Briareus.Logic.Evaluation import DeclareFact, Fact, FactList, run_logic_analysis, LogicOutput
from Briareus.Logic.InpFacts import get_input_facts
import attr
from typing import Any, List, Set, Tuple, Union


@attr.s(auto_attribs=True, frozen=True)
class GeneratedConfigs(object):
    cfg_build_configs: List[BldConfig] = attr.ib()  # This is a list of InpFacts BldConfig objects
    cfg_subrepos: Set[RepoSite] = attr.ib(factory=set)  # repo_info['subrepos']
    cfg_pullreqs: Set[PRInfo] = attr.ib(factory=set)  # repo_info['pullreqs']


BuildConfigGenTy = Union[FactList, LogicOutput, GeneratedConfigs]

class Generator(object):
    def __init__(self, actor_system=None, verbose: bool = False) -> None:
        self._actor_system = actor_system
        self.verbose = verbose

    def generate_build_configs(self, input_descr: InputDesc,
                               repo_info : RepoInfoTy,
                               up_to: UpTo = None) -> Tuple[str, BuildConfigGenTy]:
        """The core process of generating build_config information from an
           input description.  The up_to argument can request early
           return with the information "up-to" a specific point; this
           is primarily used for diagnostics and testing.
        """
        facts = get_input_facts(input_descr.PNAME,
                                input_descr.RL,
                                input_descr.BL,
                                input_descr.VAR,
                                repo_info)
        if self.verbose or up_to == 'facts':
            print_each('FACTS', facts)
        if up_to == "facts":
            return ("facts", facts)
        r = run_logic_analysis('build_config', facts,
                               actor_system=self._actor_system,
                               verbose=self.verbose)
        if self.verbose or up_to == 'raw_logic_output':
            print_titled('RAW_LOGIC_OUTPUT', r)
        if up_to == "raw_logic_output":
            return ("raw_logic_output", r)
        if not r:
            return ("no logic output", [])

        return ("build_configs",
                GeneratedConfigs(eval(r, globals(), logic_result_expr),
                                 set(repo_info.info_subrepos),
                                 set(repo_info.info_pullreqs)))
