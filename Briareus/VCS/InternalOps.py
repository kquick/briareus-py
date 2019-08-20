# Internal functionality for managing VCS Repo interactions.

from thespian.actors import *
from thespian.initmsgs import initializing_messages
from Briareus.BCGen.Description import RepoDesc
from Briareus.VCS.InternalMessages import *
from Briareus.VCS.GitRepo import GitRepoInfo
import attr


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


class GatherRepoInfo(ActorTypeDispatcher):
    """Gathers information for the list of repos and branches provided in
    the input GatherInfo message, sending the information back in the
    GatheredInfo response message.

    Uses sub-actors to get information, and recursively adds new
    requests as new repositories, branches, and pull requests are
    discovered during the information gathering.
    """

    def __init__(self, *args, **kw):
        super(GatherRepoInfo, self).__init__(*args, **kw)
        self._get_git_info = None
        self.top_requestor = None

    def get_git_info(self, reqmsg):
        if not self._get_git_info:
            self._get_git_info = self.createActor(GetGitInfo, globalName="GetGitInfo")
            self.send(self._get_git_info, VCSConfig(self.cachedir, self.request_auth))
        self.responses_pending += 1
        self.send(self._get_git_info, reqmsg)


    def got_response(self, got_a_response=True):
        if got_a_response and self.responses_pending:
            self.responses_pending -= 1
        if self.responses_pending == 0:
            if self.top_requestor:
                self.send(self.top_requestor,
                          GatheredInfo({ "pullreqs" : self.pullreqs,
                                         "submodules": self.submodules,
                                         "subrepos" : self.subrepos,
                                         "branches" : self.branches
                          }))
            self.top_requestor = None

    def receiveMsg_ChildActorExited(self, msg, sender):
        if msg.childAddress == self._get_git_info:
            self._get_git_info = None
            if self.top_requestor:
                self.send(self.top_requestor,
                          GatheredInfo(None, 'GitInfo actor exited'))
                self.top_requestor = None

    def receiveMsg_InvalidRepo(self, msg, sender):
        if self.top_requestor:
            self.send(self.top_requestor,
                      GatheredInfo(None, 'Invalid %s repo "%s", remote %s, local %s: %s' %
                                   (msg.repo_type, msg.reponame, msg.repo_remote, msg.repo_localdir,
                                    msg.errorstr)))
            self.top_requestor = None

    def receiveMsg_GatherInfo(self, msg, sender):
        self.top_requestor = sender
        self.responses_pending = 0

        self.pullreqs = set()
        self.submodules = set()
        self.subrepos = set()
        self.branches = set()
        self._pending_info = {}

        self.RL = msg.repolist
        self.BL = msg.branchlist
        self.cachedir = msg.cachedir
        self.request_auth = msg.request_auth
        for repo in self.RL:
            self.get_info_for_a_repo(repo)
        self.got_response(False)

    def get_info_for_a_repo(self, repo):
        self.get_git_info(DeclareRepo(repo.repo_name, repo.repo_url))
        self._pending_info[repo.repo_name] = repo

    def receiveMsg_RepoDeclared(self, msg, sender):
        repo = self._pending_info.get(msg.reponame, None)
        if repo:
            del self._pending_info[msg.reponame]
            self.get_git_info(GetPullReqs(repo.repo_name))
            for branch in self.BL:
                self.get_git_info(HasBranch(repo.repo_name, branch.branch_name))
        self.got_response()

    def receiveMsg_PullReqsData(self, msg, sender):
        # A pull request references a branch in a different repo
        # where that branch exists; the branch does not exist in
        # the current repo.  For github, there is also a
        # "pullreq_mergeref", which is a specific sha for a test
        # merge commit for that repo; this is not used here
        # because it is only available for automatic merges and
        # represents a step not yet taken for the workflow.
        #
        # Note that this branch probably does not exist on the
        # main repo; if it does, it's considered to be different
        # than the pull request, so don't compare it to a local
        # branch (i.e. build both if specified).  By extension,
        # there can be multiple PR's for the same branch name from
        # two separate source repositories: again, these are
        # distinct.
        #
        # For compatibility with this PR, other repos might have a
        # branch or a PR with the same name (or both!).  Note that the
        # latter supercedes the former, and the latter is already
        # handled by the pullreqs retrievals, so just check for
        # branches.
        for p in msg.pullreqs:
            # If this pull request is against the master of the source
            # repo, there is no branch association to be made since
            # all repos have a master.
            if p.pullreq_branch == "master":
                continue
            # If this is a new pull request branch, and that branch
            # has not already been probed for the target repo, check
            # to see if the branch exists (and if it is confirmed to
            # exist and it's the project repo, also get any submodule
            # data on that branch).
            for pr in self.pullreqs:
                if p.pullreq_branch == pr.pr_branch:
                    break
            else:
                for repo in (list(self.RL) + list(self.subrepos)):
                    # don't bother to check the repo where the pullreq was found
                    if repo.repo_name != msg.reponame:
                        curbr = (repo.repo_name, p.pullreq_branch)
                        for br in self.branches:
                            if curbr == br:
                                break
                        else:
                            self.get_git_info(HasBranch(repo.repo_name, p.pullreq_branch))
                    elif repo.project_repo:
                        self.get_git_info(GitmodulesData(repo.repo_name,
                                                         p.pullreq_branch,
                                                         alt_repo_url=p.pullreq_srcurl))

        self.pullreqs.update(set([PRInfo(pr_target_repo=msg.reponame,
                                         pr_srcrepo_url=p.pullreq_srcurl,
                                         pr_branch=p.pullreq_branch,
                                         pr_ident=p.pullreq_number,
                                         pr_title=p.pullreq_title)
                                  for p in msg.pullreqs]))
        self.got_response()

    def receiveMsg_BranchPresent(self, msg, sender):
        if msg.branch_present:
            self.branches.add( (msg.reponame, msg.branch_name) )
            for repo in self.RL:
                if repo.project_repo and repo.repo_name == msg.reponame:
                    # This is a branch on the project repo, so see if
                    # there is any submodule information on that
                    # branch.
                    self.get_git_info(GitmodulesData(repo.repo_name, msg.branch_name))
        self.got_response()

    def receiveMsg_GitmodulesRepoVers(self, msg, sender):
        for each in msg.gitmodules_repovers:
            named_submod_repo = ([r for r in (list(self.RL) + list(self.subrepos))
                                  if r.repo_name == each.subrepo_name] + [None])[0]
            if not named_submod_repo:
                # Note: assumes a subrepo URL doesn't change across parent branches
                named_submod_repo = RepoDesc(each.subrepo_name, each.subrepo_url)
                self.subrepos.add(named_submod_repo)
                self.get_info_for_a_repo(named_submod_repo)
            # Add the submodule specification for this submodule repo
            # and any other modules that share the same repo
            for r in self.RL:
                if r.repo_url == named_submod_repo.repo_url:
                    self.submodules.add( SubModuleInfo(sm_repo_name=msg.reponame,
                                                       sm_branch=msg.branch_name,
                                                       sm_sub_name=r.repo_name,
                                                       sm_sub_vers=each.subrepo_vers,
                                                       sm_alt_repourl=msg.alt_repo_url) )
            for r in self.subrepos:
                if r.repo_url == named_submod_repo.repo_url:
                    self.submodules.add( SubModuleInfo(sm_repo_name=msg.reponame,
                                                       sm_branch=msg.branch_name,
                                                       sm_sub_name=r.repo_name,
                                                       sm_sub_vers=each.subrepo_vers,
                                                       sm_alt_repourl=msg.alt_repo_url) )
            # Now check for PR-driven branches in those subrepos
            for pr in self.pullreqs:
                # For any pull requests that have already been fetched:
                # check to see if there is a corresponding branch in
                # this subrepo
                curbr = (msg.reponame, pr.pr_branch)
                for br in self.branches:
                    if curbr == br:
                        break
                else:
                    self.get_git_info(HasBranch(msg.reponame, pr.pr_branch))
        self.got_response()


@initializing_messages([('config', VCSConfig)])
class GetGitInfo(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GetGitInfo, self).__init__(*args, **kw)
        self.gitinfo_actors = {}
        self.gitinfo_actors_by_url = {}

    def _get_subactor(self, reponame, repourl=None):
        suba = self.gitinfo_actors.get(reponame, None)
        if not suba:
            if not repourl:
                raise RuntimeError('No URL for defined repo %s' % reponame)  # KWQ:
            # make a message Optimization: sometimes different modules
            # share a repo (they are different subdirectories).  In
            # this case, the queries are at the repo level, so there's
            # no need to query multiple times.  If there is already an
            # entry for this repo URL, just re-use it.
            if repourl in self.gitinfo_actors_by_url:
                self.gitinfo_actors[reponame] = self.gitinfo_actors_by_url[repourl]
                return self.gitinfo_actors[reponame]
            suba = self.createActor(GitRepoInfo)
            self.gitinfo_actors[reponame] = suba
            self.gitinfo_actors_by_url[repourl] = suba
            self.send(suba, RepoRemoteSpec(repourl,
                                           cachedir=self.config.cachedir_root,
                                           request_auth=self.config.request_auth))
        return suba

    def receiveMsg_ActorExitRequest(self, msg, sender):
        for each in self.gitinfo_actors.values():
            self.send(each, msg)

    def receiveMsg_DeclareRepo(self, msg, sender):
        suba = self._get_subactor(msg.reponame, msg.repo_url)
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_Repo__ReqMsg(self, msg, sender):
        suba = self._get_subactor(msg.reponame)
        msg.orig_sender = sender
        self.send(suba, msg)

    def receiveMsg_str(self, msg, sender):
        if msg == "status":
            self.send(sender, self.gitinfo_actors)
