# Messages exchanged between VCS management actors

import attr


@attr.s
class GatherInfo(object):             #            --> GatheredInfo
    repolist = attr.ib(factory=list)    # array of RepoDesc
    branchlist = attr.ib(factory=list)  # array of BranchDesc
    cachedir = attr.ib(default=None)  # local directory root for caching repos
    request_auth = attr.ib(default=None)  # interpreted by VCS/GitRepo
@attr.s
class GatheredInfo(object):           # GatherInfo -->
    info = attr.ib(factory=dict)
    error = attr.ib(default=None)

@attr.s
class VCSConfig(object):
    cachedir_root = attr.ib()
    request_auth = attr.ib(default=None)   # interpreted by VCS/GitRepo


@attr.s
class Repo__ReqMsg(object):
    reponame = attr.ib()
@attr.s
class Repo__RspMsg(object):
    reponame = attr.ib()


@attr.s
class DeclareRepo(Repo__ReqMsg):        #             --> RepoDeclared
    repo_url = attr.ib()
@attr.s
class RepoDeclared(Repo__RspMsg):       # DeclareRepo -->
    pass


@attr.s
class InvalidRepo(Repo__RspMsg):        # {ANY} -->
    repo_type = attr.ib()
    repo_remote = attr.ib()
    repo_localdir = attr.ib()
    errorstr = attr.ib()


class GetPullReqs(Repo__ReqMsg): pass   #             --> PullReqInfo
@attr.s
class PullReqsData(Repo__RspMsg):       # GetPullReqs -->
    pullreqs = attr.ib(factory=list)    # list of PullReqInfo

@attr.s
class PullReqInfo(object):
    pullreq_number = attr.ib()
    pullreq_title  = attr.ib()
    pullreq_srcurl = attr.ib()
    pullreq_branch = attr.ib()
    pullreq_mergeref = attr.ib(default=None) # if available

@attr.s
class HasBranch(Repo__ReqMsg):          #           --> BranchPresent
    branch_name = attr.ib()
@attr.s
class BranchPresent(Repo__RspMsg):      # HasBranch -->
    branch_name = attr.ib()
    branch_present = attr.ib(default=False)

@attr.s
class GitmodulesData(Repo__ReqMsg):     #                --> GitmodulesRepoVers
    branch_name = attr.ib()
    alt_repo_url = attr.ib(default=None)  # if in another repo (e.g. for a PullReq)
@attr.s
class GitmodulesRepoVers(Repo__RspMsg): # GitmodulesData -->
    branch_name = attr.ib()
    gitmodules_repovers = attr.ib(factory=list)  # array of SubRepoVers
    alt_repo_url = attr.ib(default=None)  # if in another repo (e.g. for a PullReq)

@attr.s
class SubRepoVers(object):
    subrepo_name = attr.ib()
    subrepo_url  = attr.ib()
    subrepo_vers = attr.ib()

@attr.s
class RepoRemoteSpec(object):
    repourl  = attr.ib()
    cachedir = attr.ib(default=None)
    request_auth = attr.ib(default=None)   # interpreted by VCS/GitRepo
