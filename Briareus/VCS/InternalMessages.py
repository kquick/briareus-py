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
class GatherInfo(object):             #            --> GatheredInfo
    repolist = attr.ib(factory=list)    # array of RepoDesc
    branchlist = attr.ib(factory=list)  # array of BranchDesc
    request_auth = attr.ib(default=None)  # interpreted by VCS/GitRepo
@attr.s
class GatheredInfo(object):           # GatherInfo -->
    info = attr.ib(factory=dict)
    error = attr.ib(default=None)

@attr.s
class VCSConfig(object):
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
    known_branches = attr.ib(factory=list)

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
    request_auth = attr.ib(default=None)   # interpreted by VCS/GitRepo


@attr.s(frozen=True)
class PRInfo(object):
    pr_target_repo = attr.ib()
    pr_srcrepo_url = attr.ib()
    pr_branch      = attr.ib()
    pr_ident       = attr.ib()  # unique identifier, required
    pr_title       = attr.ib()  # user-assistance, optional


@attr.s(frozen=True)
class SubModuleInfo(object):
    sm_repo_name = attr.ib()
    sm_branch    = attr.ib()
    sm_sub_name  = attr.ib()
    sm_sub_vers  = attr.ib()
    sm_alt_repourl = attr.ib(default=None)  # set if this came from an alt repo (eg. PullReq src repo)
