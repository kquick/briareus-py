# Data representation of build configurations and report data
#
# ALL Logic output items are translated back into Python objects and
# values defined here via the logic_result_expr translation below.

import attr

@attr.s(frozen=True)
class BldConfig(object):
    projectname = attr.ib()
    branchtype = attr.ib()  # default="regular"
    branchname = attr.ib()
    strategy   = attr.ib()  # default="standard"
    description= attr.ib()  # PR_Solo, PR_Repogroup, PR_Grouped, BranchReq, MainBranch (or string placeholder)
    blds       = attr.ib(factory=frozenset, converter=frozenset)  # list of BldRepoRev
    bldvars    = attr.ib(factory=frozenset, converter=frozenset)  # list of BldVariable

@attr.s(frozen=True)
class BranchReq(object):
    projectname = attr.ib() # string
    branchname  = attr.ib() # string

@attr.s(frozen=True)
class MainBranch(object):
    projectname = attr.ib() # string
    branchname  = attr.ib() # string

def pr_type(typespec, *args):
    return eval(typespec)(*args)

@attr.s(frozen=True)
class PR_Solo(object):
    reponame   = attr.ib() # string
    pullreq_id = attr.ib() # string, never project_primary

@attr.s(frozen=True)
class PR_Repogroup(object):
    pullreq_id = attr.ib() # string, never project_primary
    reponames  = attr.ib() # [string]

@attr.s(frozen=True)
class PR_Grouped(object):
    branchname = attr.ib() # string

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
# Reporting

@attr.s(frozen=True)
class ProjectSummary(object):
    project_name = attr.ib()
    bldcfg_count = attr.ib()  # int
    subrepo_count = attr.ib()  # int
    pullreq_count = attr.ib()  # int

@attr.s(frozen=True)
class StatusReport(object):
    status    = attr.ib()  # string "succeeded", "fixed", "initial_success", "pending", "badconfig", or int count of failing build jobs.
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
# Analysis

@attr.s(frozen=True)
class MergeablePR(object):
    branch = attr.ib() # string: branch name
    repo_and_pr_id = attr.ib() # array of tuples of repo_holding_pr and pr_id_in_that_repo

@attr.s(frozen=True)
class SepHandledVar(object):
    project = attr.ib() # string: project repo
    var_name = attr.ib() # string: variable name
    var_value = attr.ib() # string: variable value

# ----------------------------------------------------------------------
# Actions

@attr.s(frozen=True)
class Notify(object):
    what = attr.ib() # string: type of notification
    item = attr.ib() # string: notification item (branch, project, etc.)
    params = attr.ib() # list of parameters associated with item


@attr.s(frozen=True)
class SendEmail(object):
    recipients = attr.ib(converter=lambda l: sorted(list(set(l)))) # list of email addresses
    notification = attr.ib() # Notify object
    sent_to = attr.ib(converter=lambda l: sorted(list(set(l)))) # list of addresses message has been sent to already

@attr.s(frozen=True)
class PostChatMessage(object):
    channels = attr.ib() # list of channels
    what = attr.ib() # Notification type
    posted = attr.ib() # list of channels message has been posted to already

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
    "branchreq": BranchReq,
    "is_main_branch": MainBranch,
    "pr_type": pr_type,
    "pr_solo": "PR_Solo",
    "pr_repogroup": "PR_Repogroup",
    "pr_grouped": "PR_Grouped",
    "bld": lambda *args: BldRepoRev(*args),
    "brr": lambda n: n,
    "varvalue": lambda *args: BldVariable(*args),

    "status_report": StatusReport,
    "succeeded": "succeeded",
    "failed": "failed",
    "fixed": "fixed",
    "initial_success": "initial_success",
    "badconfig": "bad_config",
    "pending": "pending",
    "var_failure": lambda *args: VarFailure(*args),
    "pr_success": lambda *args: PR_Success(*args),
    "pr_failure": lambda *args: PR_Failure(*args),
    "pr_failing": lambda *args: PR_Failing(*args),  # KWQ: old
    "config_error": lambda *args: ConfigError(*args),
    "complete_failure": lambda *args: CompletelyFailing(*args),

    "mergeable_pr": lambda *args: MergeablePR(*args),
    "var_handled_separately": lambda *args: SepHandledVar(*args),

    "notify": lambda *args: Notify(*args),
    "email": lambda *args: SendEmail(*args),
    "chat": lambda *args: PostChatMessage(*args),
    "merge_pr": "merge_pr",
    "completely_broken": "completely_broken",
    "main_submodules_broken": "main_submodules_broken",
    "main_submodules_good": "main_submodules_good",
    "main_good": "main_good",
    "main_broken": "main_broken",
    "variable_failing": "variable_failing",


    "project": lambda n: n,
}
