from thespian.actors import *
from Briareus.VCS.InternalMessages import *

class GitExample2(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitExample2, self).__init__(*args, **kw)

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        ### EXAMPLE-vvv
        preqs = []
        ### EXAMPLE-^^^
        self.send(sender, PullReqsData(msg.reponame, preqs))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        repo_branches = {
            'Repo1': [ 'develop', 'misc', 'stuff/here' ],
            'Repo2': [ 'develop', 'humdrum' ],
            'Repo3': [ 'develop' ],
            'Repo4': [ ],
        }[msg.reponame] + ['master']
        # All repos have a master branch
        chk = branch in repo_branches
        ### EXAMPLE-^^^
        self.send(sender, BranchPresent(msg.reponame, branch, chk,
                                        known_branches=repo_branches))

    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        rsub = {
            'Repo1': { "master":[SubRepoVers('Repo2', "r2_url", "r2_master_head"),
                                 SubRepoVers('Repo3', "r3_url", "r3_master_head^3"),
                                 SubRepoVers('Repo4', "r4_url", "r4_master_head^1")],
                       'develop':[SubRepoVers('Repo2', "r2_url", "r2_develop_head"),
                                  SubRepoVers('Repo3', "r3_url", "r3_develop_head"),
                                  SubRepoVers('Repo4', "r4_url", "r4_master_head")],
            },
        }[msg.reponame]
        rval = rsub.get(branch, [])
        if msg.reponame == 'Repo1':
            # If a pull req, might have different repos that only
            # exist in submodules from that pull req; see git_example1.
            pass
        ### EXAMPLE-^^^
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, msg.pullreq_id, rval))
