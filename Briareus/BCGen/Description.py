# Static Data Description of the Briareus Input Specifications

import attr

@attr.s(frozen=True)
class InputDesc(object):
    RL = attr.ib(factory=list)  # repo list
    BL = attr.ib(factory=list)  # branch list
    VAR = attr.ib(factory=list) # variables list

@attr.s(frozen=True)
class RepoDesc(object):
    repo_name = attr.ib()
    repo_url = attr.ib()
    project_repo = attr.ib(default=False)

@attr.s(frozen=True)
class BranchDesc(object):
    branch_name = attr.ib()


@attr.s(frozen=True)
class VariableDesc(object):
    variable_name = attr.ib()
    variable_values = attr.ib()  # list of names
