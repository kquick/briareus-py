# Messages exchanged between VCS management actors

import attr
import json
from Briareus.VCS_API import *
from .ForgeAccess import (RepoAPI_Location, API_URL)
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


class SAME_URL(object):
    "Same URL as referencing URL, but latter isn't known when this is set."

@attr.s(auto_attribs=True, frozen=True)
class DIFFERENT_URL(object):
    """URL is different than the current URL, but there is not enough
       information to get an actual URL.  This can happen for gitforge
       merge request source URLs.
    """
    reponame: str
    urltype: str = 'DURL'

@attr.s(auto_attribs=True, frozen=True)
class BOGUS_URL(object):
    """Not an actual URL but something that was synthesized internally as
       a placeholder.  The placeholder should never actually be used."""
    reason: str
    urltype: str = 'BOGUS'


@attr.s(auto_attribs=True)
class Repo__ReqMsg(object):
    reponame: str
@attr.s(auto_attribs=True)
class Repo__RspMsg(object):
    reponame: str


@attr.s(auto_attribs=True)
class ReadFileFromVCS(Repo__ReqMsg):        # --> FileReadData
    repourl: UserURL
    repolocs: List[SSHHostName]
    file_path: str
    branch: str = attr.ib(factory=str)     # defaults to "master"
@attr.s(auto_attribs=True)
class FileReadData(Repo__RspMsg):           # ReadFileFromVCS -->
    req: ReadFileFromVCS                # The ReadFileFromVCS that originated this response
    error_code: Optional[int] = attr.ib(default=None)  # integer, 0/None on success
    file_data: str = attr.ib(factory=str)    # empty string if error_code


@attr.s(auto_attribs=True)
class Repo_AltLoc_ReqMsg(object):
    """Sends a Repo__ReqMsg but the target should handle it at an
       alternate repository that is associated with the main reponame
       but is not the main repo (e.g. a pullreq source repo).
    """
    api_repo_loc: RepoAPI_Location   # API URL specification for alt repo
    altloc_reqmsg: Repo__ReqMsg      # Repo__ReqMsg to send to the alt repo loc


@attr.s(auto_attribs=True)
class DeclareRepo(Repo__ReqMsg):        #             --> RepoDeclared
    repo_url: UserURL
    repolocs: List[SSHHostName] = attr.ib(factory=list)
@attr.s
class RepoDeclared(Repo__RspMsg):       # DeclareRepo -->
    pass


@attr.s(auto_attribs=True)
class InvalidRepo(Repo__RspMsg):        # {ANY} -->
    repo_name: str
    repo_type: str
    repo_api_url: str
    errorstr: str


@attr.s(auto_attribs=True)
class PullReqInfo(object):
    pullreq_number: str
    pullreq_status: PullReqStatus__Base # derivation
    pullreq_title: str
    pullreq_srcurl: Union[UserURL, SSH_URL, SAME_URL, DIFFERENT_URL, BOGUS_URL]
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


@attr.s(auto_attribs=True)
class SubRepoVers(object):
    subrepo_name: str
    subrepo_url: Union[UserURL, BOGUS_URL]
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
    repo_api_loc: RepoAPI_Location   # for forge API


InfoReturnTy = Union[List[BranchRef],
                     List[SubModuleInfo],
                     List[RepoSite],
                     List[PRInfo]]

@attr.s(auto_attribs=True)
class GatherInfo(object):             #            --> GatheredInfo
    repolist: List[RepoSite] = attr.ib(factory=list)
    repolocs: List[SSHHostName]  = attr.ib(factory=list)
    branchlist: List[BranchName] = attr.ib(factory=list)
@attr.s(auto_attribs=True)
class GatheredInfo(object):           # GatherInfo -->
    branches: List[BranchRef]
    submodules: List[SubModuleInfo]
    subrepos: List[RepoSite]
    pullreqs: List[PRInfo]
    error: Optional[str] = attr.ib(default=None)
