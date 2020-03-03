# Messages exchanged between VCS management actors

import attr
import json
from Briareus.Input.Description import *

def toJSON(obj):
    class objToJSON(json.JSONEncoder):
        def default(self, obj):
            if obj.__class__.__name__ in [ 'dict', 'list', 'int', 'float',
                                           'str', 'bool', 'NoneType' ]:
                return obj
            if obj.__class__.__name__ in ['set', 'tuple']:
                return { '__type__': obj.__class__.__name__,
                         '__value__': [self.default(e) for e in obj]
                }
            objdict = attr.asdict(obj, recurse=False)
            objdict['__type__'] = obj.__class__.__name__
            return objdict
    return json.dumps(obj, cls=objToJSON)

def fromJSON(jstr):
    def objFromJSON(objdict):
        if '__type__' in objdict:
            objtype = objdict['__type__']
            if '__value__' in objdict:
                return eval(objtype)(objdict['__value__'])
            del objdict['__type__']
            return eval(objtype)(**objdict)
        return objdict
    return json.loads(jstr, object_hook=objFromJSON)


@attr.s
class RepoAPI_Location(object):
    apiloc = attr.ib()   # HTTP API URL base used internally to get
                         # information.  This may not yet be a valid
                         # API reference because there is often path
                         # or URL adjustments based on that, but this
                         # should be an http reference to an API
                         # server (e.g. a git forge) instead of an ssh
                         # or http reference to a repository.
    apitoken = attr.ib() # Token used to access the API URL (or None if no token)

@attr.s
class GatherInfo(object):             #            --> GatheredInfo
    repolist = attr.ib(factory=list)    # array of RepoDesc
    repolocs = attr.ib(factory=list)    # array of RepoLoc
    branchlist = attr.ib(factory=list)  # array of BranchDesc
@attr.s
class GatheredInfo(object):           # GatherInfo -->
    info = attr.ib(factory=dict)
    error = attr.ib(default=None)


@attr.s
class ReadFileFromVCS(object):        # --> FileReadData
    repourl  = attr.ib()
    repolocs = attr.ib()              # array of RepoLoc
    file_path = attr.ib()
    branch = attr.ib(factory=str)     # defaults to "master"
@attr.s
class FileReadData(object):           # ReadFileFromVCS -->
    req = attr.ib()                     # The ReadFileFromVCS that originated this response
    error_code = attr.ib(default=None)  # integer, 0/None on success
    file_data = attr.ib(factory=str)    # string, empty if error_code


@attr.s
class Repo__ReqMsg(object):
    reponame = attr.ib()
@attr.s
class Repo__RspMsg(object):
    reponame = attr.ib()


@attr.s
class Repo_AltLoc_ReqMsg(object):
    """Sends a Repo__ReqMsg but the target should handle it at an
       alternate repository that is associated with the main reponame
       but is not the main repo (e.g. a pullreq source repo).
    """
    api_repo_loc = attr.ib()   # API URL specification for alt repo
    altloc_reqmsg = attr.ib()  # Repo__ReqMsg to send to the alt repo loc


@attr.s
class DeclareRepo(Repo__ReqMsg):        #             --> RepoDeclared
    repo_url = attr.ib()
    repolocs = attr.ib(factory=list)    # array of RepoLoc
@attr.s
class RepoDeclared(Repo__RspMsg):       # DeclareRepo -->
    pass


@attr.s
class InvalidRepo(Repo__RspMsg):        # {ANY} -->
    repo_type = attr.ib()
    repo_remote = attr.ib()
    repo_api_url = attr.ib()
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
    pullreq_ref    = attr.ib()
    pullreq_user   = attr.ib() # string name of user on forge
    pullreq_email  = attr.ib() # string user email (may be blank if not public)
    pullreq_mergeref = attr.ib(default=None) # if available

@attr.s
class HasBranch(Repo__ReqMsg):          #           --> BranchPresent
    branch_name = attr.ib()
@attr.s
class BranchPresent(Repo__RspMsg):      # HasBranch -->
    branch_name = attr.ib()
    branch_present = attr.ib(default=False)
    known_branches = attr.ib(factory=list)

@attr.s
class GitmodulesData(Repo__ReqMsg):     #                --> GitmodulesRepoVers
    branch_name = attr.ib()
    pullreq_id = attr.ib()  # may be none, but distinguishes between
                            # PR's from master branches.  The ID was
                            # chosen to be unique relative to the
                            # source repo; another unique field
                            # (e.g. pullreq_srcurl) could
                            # alternatively be used.
    source_ref = attr.ib()  # explicit ref for src (e.g. for Gitlab)
@attr.s
class GitmodulesRepoVers(Repo__RspMsg): # GitmodulesData -->
    branch_name = attr.ib()
    pullreq_id = attr.ib()  # may be none, but distinguishes between PR's from master branches
    gitmodules_repovers = attr.ib(factory=list)  # array of SubRepoVers

@attr.s
class SubRepoVers(object):
    subrepo_name = attr.ib()
    subrepo_url  = attr.ib()
    subrepo_vers = attr.ib()

@attr.s
class RepoRemoteSpec(object):
    repo_api_loc = attr.ib()               # RepoAPI_Location for forge API


@attr.s(frozen=True)
class PRInfo(object):
    pr_target_repo = attr.ib()
    pr_srcrepo_url = attr.ib()
    pr_branch      = attr.ib()
    pr_revision    = attr.ib()
    pr_ident       = attr.ib()  # unique identifier string, required
    pr_title       = attr.ib()  # user-assistance, optional
    pr_user        = attr.ib()  # name of user creating pull request
    pr_email       = attr.ib()  # email of user (if known, else blank)


@attr.s(frozen=True)
class SubModuleInfo(object):
    """Describes a known submodule for a repository.  Only a project
       repository is checked for submodules.

       Note that a Submodule specifies a subrepo name and version, and
       that since a VCS repository may contain several modules, that a
       single VCS submodule reference might create multiple
       SubModuleInfo objects---one for each module---which differ only
       in sm_sub_name.

       The repo to which this submodule reference belongs is
       identified by the tuple of: repo_name, branch, and pullreq_id.
       Note that any branch or pullreq might change the submodule
       specification.  The pullreq_id may be None, indicating that
       this is a branch in the main project repo; if the pullreq_id is
       not None, it is the ID of the pullreq (one or more pullreq
       source repos might use the same branch name, so the pullreq_id
       is used to uniquely differentiante between these).

    """
    sm_repo_name = attr.ib()  # Primary repo name (the one that contains the submodule)
    sm_branch    = attr.ib()  # Submodule branch name (in source repo)
    sm_pullreq_id= attr.ib()  # If submodule for a PR to the project repo, this identifies the PR
    sm_sub_name  = attr.ib()  # Submodule repo name
    sm_sub_vers  = attr.ib()  # Submodule repo specified version
