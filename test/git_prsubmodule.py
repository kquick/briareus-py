from thespian.actors import *
from Briareus.VCS.InternalMessages import *
from collections import defaultdict

"""This test ensures that a PR that only exists in a submodule (not a
declared module) is properly considered for build configurations."""

class GitPRSub(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitPRSub, self).__init__(*args, **kw)
        self.main_branches = {}
        self.pullreqs = {
            "SubRepo1": [
                PullReqInfo("pr312",
                            pullreq_title='better',
                            pullreq_srcurl='subrepo1_pr312_loc',
                            pullreq_branch='subfix',
                            pullreq_ref='sr1pr312sf3',
                            pullreq_user='dev',
                            pullreq_email='dev@soft.ware'),
            ],
        }
        self.gitmodules = {
            "TopRepo": {
                "master": [
                    SubRepoVers("SubRepo1", "subrepo_loc", "subrepo_master_head"),
                ],
            },
        }

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        preqs = self.pullreqs.get(msg.reponame, [])
        self.send(sender, PullReqsData(msg.reponame, preqs))

    def receiveMsg_HasBranch(self, msg, sender):
        self.send(sender,
                  BranchPresent(msg.reponame, msg.branch_name,
                                msg.branch_name ==
                                self.main_branches.get(msg.reponame,
                                                       "master")))

    def receiveMsg_GitmodulesData(self, msg, sender):
        self._gitmodules_data(msg, sender, '')

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        assert isinstance(msg.altloc_reqmsg, GitmodulesData)
        self._gitmodules_data(msg.altloc_reqmsg, sender, msg.api_repo_loc)

    def _gitmodules_data(self, msg, sender, alt_repo_url):
        ref = msg.source_ref or msg.branch_name
        alt_url = alt_repo_url.apiloc if alt_repo_url else alt_repo_url
        rval = self.gitmodules.get(msg.reponame, defaultdict(list)) \
                              .get(msg.branch_name, [])
        self.send(sender,
                  GitmodulesRepoVers(msg.reponame,
                                     msg.branch_name,
                                     msg.pullreq_id,
                                     rval))
