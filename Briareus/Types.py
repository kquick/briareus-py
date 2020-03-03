# Data representation of build configurations and report data
#
# ALL Logic output items are translated back into Python objects and
# values defined here via the logic_result_expr translation below.

import attr

def sorted_nub_list(l):
    return sorted(list(set(l)))

@attr.s(frozen=True)
class BldConfig(object):
    projectname = attr.ib()
    branchtype = attr.ib()  # default="regular"
    branchname = attr.ib()
    strategy   = attr.ib()  # default="standard"
    description= attr.ib()  # PR_Solo, PR_Repogroup, PR_Grouped, BranchReq, MainBranch (or string placeholder)
    blds       = attr.ib(factory=list, converter=sorted_nub_list)  # list of BldRepoRev
    bldvars    = attr.ib(factory=list, converter=sorted_nub_list)

@attr.s(frozen=True)
class BranchReq(object):
    projectname = attr.ib() # string
    branchname  = attr.ib() # string

    def as_fact(self):
        return 'branchreq("' + self.projectname + '", "' + self.branchname + '")'


@attr.s(frozen=True)
class MainBranch(object):
    reponame = attr.ib() # string
    branchname  = attr.ib() # string

    def as_fact(self):
        return 'main_branch("' + self.reponame + '", "' + self.branchname + '")'

def pr_type(typespec, *args):
    return eval(typespec)(*args)

@attr.s(frozen=True)
class PR_Solo(object):
    reponame   = attr.ib() # string
    pullreq_id = attr.ib() # string, never project_primary

    def as_fact(self):
        return 'pr_type(pr_solo, "' + self.reponame + '", "' + self.pullreq_id + '")'

@attr.s(frozen=True)
class PR_Repogroup(object):
    pullreq_id = attr.ib() # string, never project_primary
    reponames  = attr.ib() # [string]

    def as_fact(self):
        return ('pr_type(pr_repogroup, "' + self.pullreq_id + '", ['
                + ', '.join(['"%s"'%n for n in self.reponames]) + '])')

@attr.s(frozen=True)
class PR_Grouped(object):
    branchname = attr.ib() # string

    def as_fact(self):
        return 'pr_type(pr_grouped, "' + self.branchname + '")'

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
    project = attr.ib()
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
    blddesc   = attr.ib(default="unk")  # same as BldConfig.description above

@attr.s(frozen=True)
class PendingStatus(object):
    """Just like StatusReport, but no 'status' field because there will
       (maybe) be a StatusReport from a previous build and this
       indicates there is a new build in progress.
    """
    project   = attr.ib()  # string name of project
    strategy  = attr.ib()  # string: submodules, heads, standard
    branchtype= attr.ib()  # string: regular, pullreq
    branch    = attr.ib()  # string: branch name
    buildname = attr.ib()  # string name of build on builder
    bldvars   = attr.ib(converter=sorted)  # list of BldVariable
    blddesc   = attr.ib(default="unk")  # same as BldConfig.description above

@attr.s(frozen=True)
class NewPending(object):
    bldcfg = attr.ib()  # BldConfig

@attr.s(frozen=True)
class VarFailure(BldVariable): pass

@attr.s(frozen=True)
class PR_Status(object):
    prtype = attr.ib() # PR_Solo, PR_Repogroup, or PR_Grouped
    branch = attr.ib() # string
    project = attr.ib() # string
    prcfg = attr.ib() # list of PRCfg or BranchCfg
    passing = attr.ib() # list of passing BldNames
    failing = attr.ib() # list of failing BldNames
    pending = attr.ib() # list of pending BldNames (i.e. in-progress)
    unstarted = attr.ib() # number of builds not yet started

fact_str = lambda s: '"' + s + '"'

@attr.s(frozen=True)
class PRCfg(object):
    reponame = attr.ib() # string
    pr_id    = attr.ib() # string
    branch   = attr.ib() # string
    user     = attr.ib() # string
    email    = attr.ib() # string

    def as_fact(self):
        return ''.join(['prcfg(',
                        ','.join([
                            fact_str(self.reponame),
                            fact_str(self.pr_id),
                            fact_str(self.branch),
                            fact_str(self.user),
                            fact_str(self.email),
                            ]),
                        ')'])

@attr.s(frozen=True)
class BranchCfg(object):
    reponame = attr.ib() # string
    branch   = attr.ib() # string

    def as_fact(self):
        return ''.join(['prcfg(',
                        fact_str(self.reponame), ',',
                        fact_str(self.branch),
                        ')'])

@attr.s(frozen=True)
class PRData(object):
    prtype = attr.ib() # PR_Solo, PR_Repogroup, or PR_Grouped
    prcfg = attr.ib() # list of PRCfg or BranchCfg

    def as_fact(self):
        return ''.join(['prdata(',
                        self.prtype.as_fact(), ',',
                        '[', ','.join([c.as_fact() for c in self.prcfg]), ']',
                        ')'])


@attr.s(frozen=True)
class PRFailData(object):
    prtype = attr.ib() # PR_Solo, PR_Repogroup, or PR_Grouped
    prcfg = attr.ib() # list of PRCfg or BranchCfg
    goods = attr.ib() # list of BuildNames
    fails = attr.ib() # list of BuildNames

    def as_fact(self):
        return ''.join(['prfaildata(',
                        self.prtype.as_fact(), ',',
                        '[', ','.join([c.as_fact() for c in self.prcfg]), '],',
                        '[', ','.join([fact_str(bn) for bn in self.goods]), '],',
                        '[', ','.join([fact_str(bn) for bn in self.fails]), ']',
                        ')'])

@attr.s(frozen=True)
class CompletelyFailing(object):
    project = attr.ib() # string name of project

# ----------------------------------------------------------------------
# Analysis

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
    subject = attr.ib() # string: project
    params = attr.ib() # parameters associated with item; specific to 'what'


@attr.s(frozen=True)
class SendEmail(object):
    recipients = attr.ib(converter=sorted_nub_list) # list of email addresses
    notification = attr.ib() # Notify object
    sent_to = attr.ib(converter=sorted_nub_list) # list of addresses message has been sent to already

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
    "main_branch": MainBranch,
    "pr_type": pr_type,
    "pr_solo": "PR_Solo",
    "pr_repogroup": "PR_Repogroup",
    "pr_grouped": "PR_Grouped",
    "bld": lambda *args: BldRepoRev(*args),
    "brr": lambda n: n,
    "varvalue": lambda *args: BldVariable(*args),

    "status_report": StatusReport,
    "pending_status": PendingStatus,
    "new_pending": NewPending,
    "succeeded": "succeeded",
    "failed": "failed",
    "fixed": "fixed",
    "initial_success": "initial_success",
    "badconfig": "bad_config",
    "bad_config": "bad_config",
    "pending": "pending",
    "var_failure": lambda *args: VarFailure(*args),
    "pr_status": PR_Status,
    "prcfg": PRCfg,
    "branchcfg": BranchCfg,
    "pr_projstatus_pending": "pr_projstatus_pending",
    "pr_projstatus_good": "pr_projstatus_good",
    "pr_projstatus_fail": "pr_projstatus_fail",
    "prdata": PRData,
    "prfaildata": PRFailData,
    "complete_failure": lambda *args: CompletelyFailing(*args),

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


# ----------------------------------------------------------------------
# Information accumulated and managed internally to Briareus

@attr.s
class RunContext(object):
    "Stores the result of one or more generated build configurations"

    actor_system = attr.ib()
    result_sets = attr.ib(factory=list) # list of ResultSet
    report = attr.ib(factory=list)  # list of report-generated items

    def add_results(self, builder, inp_desc, repo_info, build_cfgs):
        self.result_sets.append(ResultSet(builder, inp_desc, repo_info, build_cfgs))



@attr.s
class ResultSet(object):
    """This object accumulates the various project-specific information
    obtained during the Briareus processing.
    """

    # At present, the builder is used by reporting to get the build
    # results, so any builder will suffice.  In the future, all
    # builders should be the same, and the first generated should be
    # threaded through, or the builder map should be passed to AnaRep
    # for consuting project-specific builders for results.
    builder = attr.ib(default=None)

    # inp_desc is an array of Briareus.Input.Description.InputDesc
    # object.  These are kept separate so that the set of input logic
    # facts are consistent to each inp_desc.
    inp_desc = attr.ib(default=None)

    # repo_info is a dictionary gathered from the remote repositories.
    # Assume that the dictionaries can simply be combined and that any
    # duplicates are identical.  This might not be true if the same
    # name were chosen for different repositories, but we currently
    # deem that a bad input configuration.
    repo_info = attr.ib(default=None)

    # build_cfgs is the internal build configs,
    # Briareus.BCGen.Generator.GeneratedConfigs
    build_cfgs = attr.ib(default=None)

    # build_results are the array of [BuildResult]
    build_results = attr.ib(factory=list)
