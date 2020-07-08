# Static Data Description of the Briareus Input Specifications

import attr
from Briareus.VCS.ForgeAccess import UserURL, RepoLoc
from typing import (Dict, List)


@attr.s(auto_attribs=True, frozen=True)
class RepoDesc(object):
    repo_name: str
    repo_url: str
    main_branch: str = attr.ib(default="master") # input optional
    project_repo: bool = attr.ib(default=False)  # internally generated, not part of the input spec

@attr.s(auto_attribs=True, frozen=True)
class BranchDesc(object):
    branch_name: str


@attr.s(auto_attribs=True, frozen=True)
class VariableDesc(object):
    variable_name: str
    variable_values: str  # list of values in string form


@attr.s(auto_attribs=True, frozen=True)
class InputDesc(object):
    RL: List[RepoDesc] = attr.ib(factory=list)
    BL: List[BranchDesc] = attr.ib(factory=list)
    VAR: List[VariableDesc] = attr.ib(factory=list)
    RX: List[RepoLoc] = attr.ib(factory=list)   # repo location translations

    # Dictionary of reporting items.  Supported entries:
    #
    #   * "logic": value is added to the reporting phase logic input.
    #     These are assumed to be facts controlling the enabling of
    #     actions.  For example,
    #
    #        '''email_domain_whitelist("company.com").
    #           enable(forge_status, _, _).
    #        '''
    #
    #   * "status_url": When generating reports or actions that
    #     reference a specific reported build status, the default URL
    #     is the builder's project page.  This value can be used to
    #     override that default; the value is passed to the
    #     str.format() function with named keyword arguments of:
    #     "project".
    #
    REP: Dict[str,str] = attr.ib(factory=dict)

    PNAME: str = attr.ib(factory=str)  # string "Project Name" (if blank, use project repo name)
