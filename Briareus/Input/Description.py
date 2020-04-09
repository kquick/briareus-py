# Static Data Description of the Briareus Input Specifications

import attr

@attr.s(frozen=True)
class InputDesc(object):
    RL  = attr.ib(factory=list)   # RepoDesc repo list
    BL  = attr.ib(factory=list)   # BranchDesc branch list
    VAR = attr.ib(factory=list)   # VariableDesc variables list
    RX  = attr.ib(factory=list)   # RepoLoc repo location translations

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
    REP = attr.ib(factory=dict)

    PNAME = attr.ib(factory=str)  # string "Project Name" (if blank, use project repo name)

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
    main_branch = attr.ib(default="master") # input optional
    project_repo = attr.ib(default=False)  # internally generated, not part of the input spec

@attr.s(frozen=True)
class BranchDesc(object):
    branch_name = attr.ib()


@attr.s(frozen=True)
class VariableDesc(object):
    variable_name = attr.ib()
    variable_values = attr.ib()  # list of names
