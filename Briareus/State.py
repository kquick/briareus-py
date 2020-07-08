import attr
from Briareus.Types import InputDesc, BuildResult
from Briareus.BuildSys.BuilderBase import Builder
from typing import Any, Dict, List, Optional, Union


# ----------------------------------------------------------------------
# Information accumulated and managed internally to Briareus top-level
# operational control


ReportType = List[Any]

@attr.s(auto_attribs=True)
class RunContext(object):
    "Stores the result of one or more generated build configurations"

    actor_system: Any
    result_sets: List["ResultSet"] = attr.ib(factory=list)
    report: ReportType = attr.ib(factory=list)  # list of report-generated items

    def add_results(self, builder, inp_desc, repo_info, build_cfgs) -> None:
        self.result_sets.append(ResultSet(inp_desc, builder, repo_info, build_cfgs))



@attr.s(auto_attribs=True)
class ResultSet(object):
    """This object accumulates the various project-specific information
    obtained during the Briareus processing.
    """

    # inp_desc is a Briareus.Input.Description.InputDesc object.
    # These are kept separate so that the set of input logic facts are
    # consistent to each inp_desc.
    inp_desc: InputDesc

    # At present, the builder is used by reporting to get the build
    # results, so any builder will suffice.  In the future, all
    # builders should be the same, and the first generated should be
    # threaded through, or the builder map should be passed to AnaRep
    # for consuting project-specific builders for results.
    builder: Optional[Builder] = attr.ib(default=None)

    # repo_info is a dictionary gathered from the remote repositories.
    # Assume that the dictionaries can simply be combined and that any
    # duplicates are identical.  This might not be true if the same
    # name were chosen for different repositories, but we currently
    # deem that a bad input configuration.
    repo_info: Dict[str, Any] = attr.ib(default=None)

    # build_cfgs is the internal build configs,
    # Briareus.BCGen.Generator.GeneratedConfigs
    build_cfgs: Optional[Any] = attr.ib(default=None) # Briareus.BCGen.Generator.GeneratedConfigs

    # build_results are the array of [BuildResult]
    build_results: List[Union[str, BuildResult]] = attr.ib(factory=list)
