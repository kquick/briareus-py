from thespian.actors import *
from Briareus.VCS.InternalMessages import *

class GitExample(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitExample, self).__init__(*args, **kw)

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        ### EXAMPLE-vvv
        preqs = {
            'Repo1': [
                # This PR uses "master", which is the same thing
                # as the main branch.  This should co-exist with the
                # master build.
                PullReqInfo(1, 'pr#mastermask', 'remote_Repo1', 'master', 'r1_master_maskref'),
            ],
            'Repo3': [
                # This PR is for develop, but it should co-exist with
                # the develop branch in R3; R1 and R2 develop should
                # be built against the R3 develop branch and this PR.
                PullReqInfo(2, 'pr#develop', 'remote_Repo3', 'develop', 'r3_develop_pr2'),
                # This PR has a corresponding branch in R2 it should
                # be built against.  Note also that it duplicates the
                # ID from the Repo1 PR; verify that these don't get
                # confused/combined.
                PullReqInfo(1, 'pr#foo', 'remote_Repo3_2', 'foo', 'r3_foo_pr3'),
            ],
        }.get(msg.reponame, [])
        ### EXAMPLE-^^^
        self.send(sender, PullReqsData(msg.reponame, preqs))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        repo_branches = {
            'Repo1': [ 'develop', ],
            'Repo2': [ 'develop', 'foo' ],
            'Repo3': [ 'develop' ],
        }[msg.reponame] + ['master']
        # All repos have a master branch
        chk = branch in repo_branches
        ### EXAMPLE-^^^
        self.send(sender, BranchPresent(msg.reponame, branch, chk,
                                        known_branches=repo_branches))

    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        rval = []
        ### EXAMPLE-^^^
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, rval))

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        if isinstance(msg.altloc_reqmsg, GitmodulesData):
            self.send(sender, GitmodulesRepoVers(msg.altloc_reqmsg.reponame,
                                                 msg.altloc_reqmsg.branch_name,
                                                 []))
