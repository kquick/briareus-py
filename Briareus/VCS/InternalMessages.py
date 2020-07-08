# Messages exchanged between VCS management actors

import attr
import json
from Briareus.Input.Description import *
from typing import (Any, Dict, List, Optional, Tuple, Union)

def toJSON(obj) -> str:
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

def fromJSON(jstr: str):
    def objFromJSON(objdict):
        if '__type__' in objdict:
            objtype = objdict['__type__']
            if '__value__' in objdict:
                return eval(objtype)(objdict['__value__'])
            del objdict['__type__']
            return eval(objtype)(**objdict)
        return objdict
    return json.loads(jstr, object_hook=objFromJSON)


@attr.s(auto_attribs=True)
class ReadFileFromVCS(object):        # --> FileReadData
    repourl: str
    repolocs: List[RepoLoc]
    file_path: str
    branch: str = attr.ib(factory=str)     # defaults to "master"
@attr.s(auto_attribs=True)
class FileReadData(object):           # ReadFileFromVCS -->
    req: ReadFileFromVCS                # The ReadFileFromVCS that originated this response
    error_code: Optional[int] = attr.ib(default=None)  # integer, 0/None on success
    file_data: str = attr.ib(factory=str)    # empty string if error_code


@attr.s(auto_attribs=True)
class Repo__ReqMsg(object):
    reponame: str
@attr.s(auto_attribs=True)
class Repo__RspMsg(object):
    reponame: str


@attr.s(auto_attribs=True)
class Repo_AltLoc_ReqMsg(object):
    """Sends a Repo__ReqMsg but the target should handle it at an
       alternate repository that is associated with the main reponame
       but is not the main repo (e.g. a pullreq source repo).
    """
    api_repo_loc: Any   # API URL specification for alt repo: RepoAPI_Location
    altloc_reqmsg: Repo__ReqMsg      # Repo__ReqMsg to send to the alt repo loc


@attr.s(auto_attribs=True)
class DeclareRepo(Repo__ReqMsg):        #             --> RepoDeclared
    repo_url: str
    repolocs: List[RepoLoc] = attr.ib(factory=list)
@attr.s
class RepoDeclared(Repo__RspMsg):       # DeclareRepo -->
    pass


@attr.s(auto_attribs=True)
class InvalidRepo(Repo__RspMsg):        # {ANY} -->
    repo_name: str
    repo_type: str
    repo_api_url: str
    errorstr: str


@attr.s(auto_attribs=True, frozen=True)
class PullReqStatus__Base(object):
    def as_fact(self) -> str:
        return self.__class__.__name__.lower()
class PRSts_Active(PullReqStatus__Base): pass
class PRSts_Closed(PullReqStatus__Base): pass
class PRSts_Merged(PullReqStatus__Base): pass

@attr.s(auto_attribs=True)
class PullReqInfo(object):
    pullreq_number: str
    pullreq_status: PullReqStatus__Base # derivation
    pullreq_title: str
    pullreq_srcurl: str
    pullreq_branch: str
    pullreq_ref: str
    pullreq_user: str   # name of user on forge
    pullreq_email: str  # user email (may be blank if not public)
    pullreq_mergeref: Optional[str] = attr.ib(default=None) # if available

class GetPullReqs(Repo__ReqMsg): pass   #             --> PullReqInfo
@attr.s(auto_attribs=True)
class PullReqsData(Repo__RspMsg):       # GetPullReqs -->
    pullreqs: List[PullReqInfo] = attr.ib(factory=list)


@attr.s(auto_attribs=True)
class HasBranch(Repo__ReqMsg):          #           --> BranchPresent
    branch_name: str
@attr.s(auto_attribs=True)
class BranchPresent(Repo__RspMsg):      # HasBranch -->
    branch_name: str
    branch_present: Union[bool,str] = attr.ib(default=False)  # either False or the current branch head ref (SHA)
    known_branches: List[Tuple[str,str]] = attr.ib(factory=list)  # list of tuples: (name,head_ref)


@attr.s(auto_attribs=True, frozen=True)
class BranchRef(object):
    reponame: str
    branchname: str
    branchref: str   # reference (e.g. SHA)


@attr.s(auto_attribs=True)
class SubRepoVers(object):
    subrepo_name: str
    subrepo_url: str
    subrepo_vers: str

@attr.s(auto_attribs=True)
class GitmodulesData(Repo__ReqMsg):     #                --> GitmodulesRepoVers
    branch_name: str
    pullreq_id: Optional[str]  # may be none, but distinguishes between
                               # PR's from master branches.  The ID was
                               # chosen to be unique relative to the
                               # source repo; another unique field
                               # (e.g. pullreq_srcurl) could
                               # alternatively be used.
    source_ref: Optional[str]  # explicit ref for src (e.g. for Gitlab)
@attr.s(auto_attribs=True)
class GitmodulesRepoVers(Repo__RspMsg): # GitmodulesData -->
    branch_name: str
    pullreq_id: Optional[str]  # may be none, but distinguishes
                               # between PR's from master branches
    gitmodules_repovers: List[SubRepoVers] = attr.ib(factory=list)


@attr.s(auto_attribs=True)
class RepoRemoteSpec(object):
    repo_api_loc: str               # RepoAPI_Location for forge API


@attr.s(auto_attribs=True, frozen=True)
class PRInfo(object):
    pr_target_repo: str
    pr_srcrepo_url: str
    pr_branch: str
    pr_revision: str
    pr_ident: str  # unique identifier string, required
    pr_status: PullReqStatus__Base  # derivation
    pr_title: str  # user-assistance, optional
    pr_user: str   # name of user creating pull request
    pr_email: str  # email of user (if known, else blank)


@attr.s(auto_attribs=True, frozen=True)
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
    sm_repo_name: str  # Primary repo name (the one that contains the submodule)
    sm_branch: str     # Submodule branch name (in source repo)
    sm_pullreq_id: Optional[str] # None if main repo branch, string if
                                 # submodule for a PR to the project
                                 # repo, this identifies the PR
    sm_sub_name: str   # Submodule repo name
    sm_sub_vers: str   # Submodule repo specified version


InfoReturnTy = Union[List[BranchRef],
                     List[SubModuleInfo],
                     List[RepoDesc],
                     List[PRInfo]]

@attr.s(auto_attribs=True)
class GatherInfo(object):             #            --> GatheredInfo
    repolist: List[RepoDesc] = attr.ib(factory=list)
    repolocs: List[RepoLoc]  = attr.ib(factory=list)
    branchlist: List[BranchDesc] = attr.ib(factory=list)
@attr.s(auto_attribs=True)
class GatheredInfo(object):           # GatherInfo -->
    branches: List[BranchRef]
    submodules: List[SubModuleInfo]
    subrepos: List[RepoDesc]
    pullreqs: List[PRInfo]
    error: Optional[str] = attr.ib(default=None)
