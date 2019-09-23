# Static Data Description of the Briareus Input Specifications

import attr

@attr.s(frozen=True)
class InputDesc(object):
    RL  = attr.ib(factory=list)   # repo list
    BL  = attr.ib(factory=list)   # branch list
    VAR = attr.ib(factory=list)   # variables list
    RX  = attr.ib(factory=list)   # repo location translations

@attr.s(frozen=True)
class RepoLoc(object):
    """These entries are used to translate the SSH repo access patterns
       (needed by Hydra) into the corresponding API host for the
       repository.  This is commonly needed for private repositories
       that Hydra must use an SSH Hostname config override to access
       (e.g. "git@projFoo-github:team/repo") but for which Briareus
       needs to access the forge API
       (e.g. "https://github.com/team/repo"), often using the
       BRIAREUS_PAT token.
    """
    repo_loc = attr.ib()  # string specified in netloc position for RL
    api_host = attr.ib()  # API forge access hostname

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
