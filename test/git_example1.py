from thespian.actors import *
from Briareus.VCS.InternalMessages import *

class GitExample1(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitExample1, self).__init__(*args, **kw)

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        ### EXAMPLE-vvv
        preqs = {
            'R1': [PullReqInfo(1, 'pr#19', 'remote_R1_b', 'blah', 'r1_blah_mergeref'),],
            'R2': [PullReqInfo(23, 'add fantasticness', 'remote_r2_a', 'bugfix9', 'r2_b9_mergeref')],
            'R4': [PullReqInfo(8192, 'fix ninth bug!', 'remote_R4_y', 'bugfix9', 'r1_bf9_mergeref')],
        }.get(msg.reponame, [])
        ### EXAMPLE-^^^
        self.send(sender, PullReqsData(msg.reponame, preqs))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        if branch == "master":
            # All repos have a master branch
            chk = True
        else:
            chk = branch in {
                'R1': [ 'feat1' ],
                'R2': [ 'bugfix9' ],
                'R3': [ 'blah' ],
                'R4': [ 'feat1' ],
                'R5': [ 'bugfix9', 'blah', 'dev' ],
                'R6': [ 'feat1' ],
                'R7': [],
            }[msg.reponame]
        ### EXAMPLE-^^^
        self.send(sender, BranchPresent(msg.reponame, branch, chk))

    def receiveMsg_GitmodulesData(self, msg, sender):
        self._gitmodules_data(msg, sender, '')

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        assert isinstance(msg.altloc_reqmsg, GitmodulesData)
        self._gitmodules_data(msg.altloc_reqmsg, sender, msg.api_repo_loc)

    def _gitmodules_data(self, msg, sender, alt_repo_url):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        rsub = {
            'R1': { "master":[SubRepoVers('R2', "r2_url", "r2_master_head"),
                              SubRepoVers('R3', "r3_url", "r3_master_head^3"),
                              SubRepoVers('R4', "r4_url", "r4_master_head^1")],
                    'blah':[SubRepoVers('R2', "r2_url", "r2_master_head^22"),
                            SubRepoVers('R3', "r3_url", "r3_master_head"),
                            SubRepoVers('R7', "r7_url", "r7_master_head^4")],
                    'feat1':[SubRepoVers('R2', "r2_url", "r2_master_head^1"),
                             SubRepoVers('R3', "r2_url", "r3_master_head"),
                             SubRepoVers('R4', "r4_url", "r4_feat1_head^2")],
            },
            'R2': { 'master':[SubRepoVers('R3', "r3_url", 'r3_master_head^9'),
                              SubRepoVers('R4', "r4_url", 'r4_master_head^16'),
                              SubRepoVers('R5', "r5_url", 'r5_master_head^25')],
            },
        }[msg.reponame]
        rval = rsub.get(branch, [])  # KWQ: name is derived from URL, remove name?
        if msg.reponame == 'R1':
            if branch == 'blah' and alt_repo_url != 'remote_R1_b':
                # The blah gitmodules information only exists in the alternate repo
                rval = rsub.get("master", [])
            if branch != 'blah' and alt_repo_url:
                # Otherwise if an alt_repo_url is specified, return nothing
                rval = []
        ### EXAMPLE-^^^
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, rval))
