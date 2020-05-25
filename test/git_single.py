from thespian.actors import *
from Briareus.VCS.InternalMessages import *

class GitTestSingle(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitTestSingle, self).__init__(*args, **kw)

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        self.send(sender,
                  PullReqsData(msg.reponame,
                               [PullReqInfo(134,
                                            pullreq_status=PRSts_Active(),
                                            pullreq_title='Hoppy toads',
                                            pullreq_srcurl='toad_repo_url',
                                            pullreq_branch='toad',
                                            pullreq_ref='toad_mergeref',
                                            pullreq_user='hoppy',
                                            pullreq_email=''),
                                PullReqInfo(91,
                                            pullreq_status=PRSts_Active(),
                                            pullreq_title='Croaking frogs',
                                            pullreq_srcurl='frog_repo_url',
                                            pullreq_branch='frog',
                                            pullreq_ref='frog_mergeref',
                                            pullreq_user='frog',
                                            pullreq_email='frog@lilypond.pad'),
                               ]))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        self.send(sender, BranchPresent(msg.reponame, branch,
                                        dict([('master', 'master-ref'),
                                              ('feat1', 'feat1-ref'),]).get(branch, False)))
        # Note that toad and frog are not in the branch list because
        # those exist on the remote toad_repo_url and frog_repo_url,
        # not on TheRepo.

    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, []))

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        assert isinstance(msg.altloc_reqmsg, GitmodulesData)
        self.receiveMsg_GitmodulesData(msg.altloc_reqmsg, sender)
