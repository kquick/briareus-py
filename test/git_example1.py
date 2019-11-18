from thespian.actors import *
from Briareus.VCS.InternalMessages import *

# The blah is a pullreq in R1, with pullreqs in R2, R3, and R6:
#
#   * R2 is a submodule and does not have the branch, so the
#     submodules build should honor the submodule ref, but the HEADs
#     build should build from the R2 PR.
#
#   * R3 is similar to R2 except that there is also a blah branch in
#     the R3 primary repo.  This branch will be ignored because the
#     submodule takes precedence.
#
#   * R6 is not a submodule but a top level repo, so the submodules
#     build should build from the R6 PR as well as the HEADs build.

class GitExample1(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitExample1, self).__init__(*args, **kw)
        self.main_branches = {}

    def receiveMsg_tuple(self, msg, sender):
        "Used by test programs to adjust the git responses"
        if msg[0] == "primary branch":
            self.main_branches[msg[1]] = msg[2]
            self.send(sender, "ok: %s main branch is %s" % (msg[1], msg[2]))
        else:
            self.send(sender, "UNRECOGNIZED MESSAGE!")

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        ### EXAMPLE-vvv
        preqs = {
            'R1': [PullReqInfo("1", 'pr#19', 'remote_R1_b', 'blah', 'r1_blah_mergeref'),],
            'R2': [PullReqInfo("23", 'add fantasticness', 'remote_r2_a', 'bugfix9', 'r2_b9_mergeref'),
                   PullReqInfo("1111", 'blah also', 'remote_r2_pr1111_url', 'blah', 'r2_blah_mergeref')],
            'R3': [PullReqInfo("11", 'blah started', 'remote_r3_pr11_url', 'blah', 'r3_blah_mergeref')],
            'R4': [PullReqInfo("8192", 'fix ninth bug!', 'remote_R4_y', 'bugfix9', 'r1_bf9_mergeref')],
            'R6': [PullReqInfo("111", 'blah match', 'remote_r6_pr111_url', 'blah', 'r6_blah_mergeref')],
        }.get(msg.reponame, [])
        ### EXAMPLE-^^^
        self.send(sender, PullReqsData(msg.reponame, preqs))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        ### EXAMPLE-vvv
        if self.main_branches.get(msg.reponame, "master") == branch:
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
                'R10': [],
            }[msg.reponame]
        ### EXAMPLE-^^^
        self.send(sender, BranchPresent(msg.reponame, branch, chk))

    def receiveMsg_GitmodulesData(self, msg, sender):
        self._gitmodules_data(msg, sender, '')

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        assert isinstance(msg.altloc_reqmsg, GitmodulesData)
        self._gitmodules_data(msg.altloc_reqmsg, sender, msg.api_repo_loc)

    def _gitmodules_data(self, msg, sender, alt_repo_url):
        ref = msg.source_ref or msg.branch_name
        alt_url = alt_repo_url.apiloc if alt_repo_url else alt_repo_url
        print('_gitmodules_data, repo %s, branch %s, pr_id %s, ref %s, alt_repo_url %s'
              % (msg.reponame, msg.branch_name, msg.pullreq_id, ref, alt_url) )
        ### EXAMPLE-vvv
        main_branch_R1 = self.main_branches.get("R1", "master")
        main_branch_R2 = self.main_branches.get("R2", "master")
        main_branch_R10 = self.main_branches.get("R10", "master")
        print('_gitmodules_data main_R1 = %s, main_R2 = %s, main_R10 = %s' % (main_branch_R1, main_branch_R2, main_branch_R10))

        rsub = {
            'R1': {
                main_branch_R1:[SubRepoVers('R2', "r2_url", "r2_master_head"),
                                SubRepoVers('R3', "r3_url", "r3_master_head^3"),
                                SubRepoVers('R4', "r4_url", "r4_master_head^1")
                ],
                'blah':[SubRepoVers('R2', "r2_url", "r2_master_head^22"),
                        SubRepoVers('R3', "r3_url", "r3_master_head"),
                        SubRepoVers('R7', "r7_url", "r7_master_head^4")
                ],
                'feat1':[SubRepoVers('R2', "r2_url", "r2_master_head^1"),
                         SubRepoVers('R3', "r2_url", "r3_master_head"),
                         SubRepoVers('R4', "r4_url", "r4_feat1_head^2")
                ],
            },
            'R2': {
                main_branch_R2:[SubRepoVers('R3', "r3_url", 'r3_master_head^9'),
                                SubRepoVers('R4', "r4_url", 'r4_master_head^16'),
                                SubRepoVers('R5', "r5_url", 'r5_master_head^25')
                ],
            },
            'R10': {
                main_branch_R10:[SubRepoVers('R3', 'r3_url', 'r3_master_head^9'),
                                 SubRepoVers('R4', 'r4_url', 'r4_master_head^1'),
                ],
            },
        }[msg.reponame]
        rval = rsub.get(msg.branch_name, [])
        if msg.reponame == 'R1':
            if ref in ['blah', 'r1_blah_mergeref'] and alt_url != 'remote_R1_b':
                # The blah gitmodules information only exists in the alternate repo
                rval = rsub.get(main_branch_R1, [])
            if ref not in ['blah', 'r1_blah_mergeref' ] and alt_url:
                # Otherwise if an alt_url is specified, return nothing
                rval = []
        ### EXAMPLE-^^^
        self.send(sender, GitmodulesRepoVers(msg.reponame, msg.branch_name, msg.pullreq_id, rval))
