from thespian.actors import *
from Briareus.VCS.InternalMessages import *
from collections import defaultdict


class GitScenario32(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitScenario32, self).__init__(*args, **kw)
        self.branches = { 'repo1': [ ('master', 'r1_master_ref'),
                                     ('foo', 'r1_foo_ref'),
                                   ],
                          'repo2': [ ('master', 'r2_master_ref'),
                                     ('dog', 'r2_dog_ref'),
                                   ],
                          'RepoA': [ ('master', 'rA_master_ref'),
                                     ('foo', 'rA_foo_ref'),
                                   ],
        }
        self.pullreqs = {
            "repo1": [
                PullReqInfo("pr-foo",
                            pullreq_title='Foo Do',
                            pullreq_srcurl='Repo1_prfoo_loc',
                            pullreq_status=PRSts_Active(),
                            pullreq_branch='foo',
                            pullreq_ref='r1prFooref',
                            pullreq_user='bar',
                            pullreq_email='bar@brown.cow'),
            ],
            # Was merged to master:
            "RepoA": [
                PullReqInfo("pr-foo",
                            pullreq_title='Foo Do',
                            pullreq_srcurl='RepoA_prfoo_loc',
                            pullreq_status=PRSts_Merged(),
                            pullreq_branch='foo',
                            pullreq_ref='rAprFooref',
                            pullreq_user='bar',
                            pullreq_email='bar@brown.cow'),
            ],
        }
        self.gitmodules = {
            "repo1": {
                "master": [
                    SubRepoVers("RepoA", "repoA_loc", "repoA_master_head"),
                    # SubRepoVers("repo2", "repo2_loc", "repo2_master_subhead"),
                ],
                "foo": [
                    SubRepoVers("RepoA", "repoA_loc", "repoA_foo_head"),
                    # SubRepoVers("repo2", "repo2_loc", "repo2_foo_subhead"),
                ],
            },
            "repo2": {
                "master": [
                    SubRepoVers("RepoA", "repoA_loc", "repoA_master_head"),
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
                                dict(self.branches.get(msg.reponame,
                                                       [("master", msg.reponame + "_master_ref")]))
                                .get(msg.branch_name, False)))

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
