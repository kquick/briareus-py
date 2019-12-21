# Data representation of build configurations and report data

import attr

@attr.s(frozen=True)
class BldConfig(object):
    projectname = attr.ib()
    branchtype = attr.ib()  # default="regular"
    branchname = attr.ib()
    strategy   = attr.ib()  # default="standard"
    blds       = attr.ib(factory=frozenset, converter=frozenset)  # list of BldRepoRev
    bldvars    = attr.ib(factory=frozenset, converter=frozenset)  # list of BldVariable

@attr.s(frozen=True)
class BldRepoRev(object):
    reponame   = attr.ib()
    repover    = attr.ib()
    pullreq_id = attr.ib()  # project_primary or the PR ID
      # srcident identifies the source loc in the Logic rules and is
      # used for debugging the ruleset.  As a result, it is ignored
      # for comparisons since a specific repo revision build
      # instruction is effective in the builder regardless of which
      # logic statement generated it.
    srcident   = attr.ib(default="unk", cmp=False)

@attr.s(frozen=True)
class BldVariable(object):
    projrepo = attr.ib()
    varname = attr.ib()
    varvalue = attr.ib()


# ----------------------------------------------------------------------

@attr.s(frozen=True)
class BuildResult(object):
    bldconfig = attr.ib()  # BldConfig
    results = attr.ib()    # BuilderResult

@attr.s(frozen=True)
class BuilderResult(object):
    buildname   = attr.ib()  # string name of build on builder
    nrtotal     = attr.ib()  # int
    nrsucceeded = attr.ib()  # int
    nrfailed    = attr.ib()  # int
    nrscheduled = attr.ib()  # int
    cfgerror    = attr.ib()  # bool


# ----------------------------------------------------------------------

@attr.s(frozen=True)
class ProjectSummary(object):
    project_name = attr.ib()
    bldcfg_count = attr.ib()  # int
    subrepo_count = attr.ib()  # int
    pullreq_count = attr.ib()  # int

@attr.s(frozen=True)
class StatusReport(object):
    status    = attr.ib()  # string "success" or "failed"
    project   = attr.ib()  # string name of project
    strategy  = attr.ib()  # string: submodules, heads, standard
    branchtype= attr.ib()  # string: regular, pullreq
    branch    = attr.ib()  # string: branch name
    buildname = attr.ib()  # string name of build on builder
    bldvars   = attr.ib(converter=sorted)  # list of BldVariable

@attr.s(frozen=True)
class VarFailure(BldVariable): pass

@attr.s(frozen=True)
class PR_Success(object):
    branch = attr.ib() # string: branch name
    repo_and_pr_id = attr.ib() # array of tuples of repo_holding_pr and pr_id_in_that_repo

@attr.s(frozen=True)
class PR_Failure(object):
    branch = attr.ib() # string: branch name
    repo_and_pr_id = attr.ib() # array of tuples of repo_holding_pr and pr_id_in_that_repo

@attr.s(frozen=True)
class PR_Failing(object):
    project = attr.ib() # string name of project
    branch = attr.ib() # string: branch name
    strategy  = attr.ib()  # string: submodules, heads, standard
    # bldvars   = attr.ib(converter=sorted)  # list of BldVariable
    buildnames= attr.ib()  # list of string name of builds on builder

@attr.s(frozen=True)
class ConfigError(object):
    project = attr.ib() # string name of project
    buildname = attr.ib()  # string name of build on builder

@attr.s(frozen=True)
class CompletelyFailing(object):
    project = attr.ib() # string name of project

# ----------------------------------------------------------------------
# The Prolog output is currently interpreted via an "eval({output})"
# operation, so define some terms to re-ify the eval'd string into
# Python description.

logic_result_expr = {
    "pullreq": "pullreq",
    "submodules": "submodules",
    "heads": "HEADs",
    "regular": "regular",
    "standard": "standard",
    "project_primary": "project_primary",
    "bldcfg": lambda *args: BldConfig(*args),
    "bld": lambda *args: BldRepoRev(*args),
    "brr": lambda n: n,
    "varvalue": lambda *args: BldVariable(*args),

    "status_report": StatusReport,
    "succeeded": "succeeded",
    "failed": "failed",
    "fixed": "fixed",
    "initial_success": "initial_success",
    "badconfig": "bad_config",
    "var_failure": lambda *args: VarFailure(*args),
    "pr_success": lambda *args: PR_Success(*args),
    "pr_failure": lambda *args: PR_Failure(*args),
    "pr_failing": lambda *args: PR_Failing(*args),  # KWQ: old
    "config_error": lambda *args: ConfigError(*args),
    "complete_failure": lambda *args: CompletelyFailing(*args),

    "project": lambda n: n,
}
