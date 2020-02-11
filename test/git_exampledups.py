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
                PullReqInfo("1",
                            pullreq_title='pr#mastermask',
                            pullreq_srcurl='remote_Repo1',
                            pullreq_branch='master',
                            pullreq_ref='r1_master_maskref',
                            pullreq_user='jdoe',
                            pullreq_email='jdoe@nocompany.com'),
                # This PR should be built with the corresponding dog PR in Repo3
                PullReqInfo("Req8",
                            pullreq_title='pr8 is great',
                            pullreq_srcurl='Repo1_Remote8',
                            pullreq_branch='dog',
                            pullreq_ref='r1_r8_f32',
                            pullreq_user='r.user',
                            pullreq_email=''),
                # This PR *also* uses "master".  It should be built
                # distinctly from PR1, co-exist with the "master"
                # build, and not involv PR9 from R3.  The PR number
                # also matches a PR in Repo3, but this also should not
                # cause confusion.
                PullReqInfo("2",
                            pullreq_title='pr numero dos',
                            pullreq_srcurl='remote_Repo1_pr2',
                            pullreq_branch='master',
                            pullreq_ref='r1_master_p2^head',
                            pullreq_user='jdoe',
                            pullreq_email='jdoe@nocompany.com'),
            ],
            'Repo3': [
                # This PR is for develop, but it should co-exist with
                # the develop branch in R3; R1 and R2 develop should
                # be built against the R3 develop branch and this PR.
                PullReqInfo("2",
                            pullreq_title='pr#develop',
                            pullreq_srcurl='remote_Repo3',
                            pullreq_branch='develop',
                            pullreq_ref='r3_develop_pr2',
                            pullreq_user='frank',
                            pullreq_email='frank@stein.co'),
                # This PR has a corresponding branch in R2 it should
                # be built against.  Note also that it duplicates the
                # ID from the Repo1 PR; verify that these don't get
                # confused/combined.
                PullReqInfo("1",
                            pullreq_title='pr#foo',
                            pullreq_srcurl='remote_Repo3_2',
                            pullreq_branch='foo',
                            pullreq_ref='r3_foo_pr3',
                            pullreq_user='earl',
                            pullreq_email='earl@king.wild'),
                # This PR is on master in the source repo, but because
                # master is the default branch, it should *not* be
                # built with other PR's on similar branches (notably
                # Repo1, PR1).
                PullReqInfo("9",
                            pullreq_title='pr#master3',
                            pullreq_srcurl='remote_repo3_other',
                            pullreq_branch='master',
                            pullreq_ref='r3_master_2',
                            pullreq_user='frank',
                            pullreq_email='frank@stein.co'),
                # This PR should be built with the corresponding dog PR in Repo1
                PullReqInfo("101",
                            pullreq_title='start changes',
                            pullreq_srcurl='Repo3_r3',
                            pullreq_branch='dog',
                            pullreq_ref='r3_r3^7',
                            pullreq_user='fido',
                            pullreq_email='fido@woof.grr'),
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
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, msg.pullreq_id, rval))

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        if isinstance(msg.altloc_reqmsg, GitmodulesData):
            self.send(sender, GitmodulesRepoVers(msg.altloc_reqmsg.reponame,
                                                 msg.altloc_reqmsg.branch_name,
                                                 msg.altloc_reqmsg.pullreq_id,
                                                 []))
