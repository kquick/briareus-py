from collections import defaultdict
from Briareus.VCS.GitForgeGQL import *
from Briareus.VCS.ForgeAccess import to_http_url

class AllRepos(object):
    def __init__(self, initial_repos=None):
        self._all_repos = set(initial_repos or [])
        self._added_repos = set()
    def url_for_repo(self, repo_name):
        for each in self._all_repos:
            if each.repo_name == repo_name:
                return each.repo_url
        return None
    def add_repo(self, repo_desc):
        self._all_repos.add(repo_desc)
        self._added_repos.add(repo_desc)
    def all_repos(self): return self._all_repos
    def discovered_repos(self):
        for r in self._added_repos:
            yield r


def get_repo_info(repo, input_desc, all_repos):
    apiloc = to_http_url(repo.repo_url, input_desc.RX)
    g = GitHub(repo.repo_name, apiloc) \
        if 'github' in apiloc.apiloc else \
           GitLab(repo.repo_name, apiloc)
    rinfo = g.get_VCS_info(get_submodules=repo.project_repo)
    # pprint(rinfo)
    for e in rinfo.get('subrepos', list()):
        known = all_repos.url_for_repo(e.repo_name)
        if not known:
            all_repos.add_repo(e)
    return {
        'branches': rinfo['branches'],
        'pullreqs': [
            PRInfo(pr_target_repo=repo.repo_name,
                   pr_srcrepo_url=(to_access_url(p.pullreq_srcurl,
                                                 ([r for r in input_desc.RL
                                                   if r.repo_name == repo.repo_name] + [None])[0],
                                                 input_desc.RX) or
                                   all_repos.url_for_repo(repo.repo_name)),
                   pr_branch=p.pullreq_branch,
                   pr_revision=p.pullreq_ref,
                   pr_ident=p.pullreq_number,
                   pr_status=p.pullreq_status,
                   pr_title=p.pullreq_title,
                   pr_user=p.pullreq_user,
                   pr_email=p.pullreq_email)
            for p in rinfo['pullreqs']],
        'subrepos': rinfo.get('subrepos', []),
        'submodules': [
            SubModuleInfo(sm_repo_name=e.reponame,
                          sm_branch=e.branch_name or repo.main_branch,
                          sm_pullreq_id=e.pullreq_id,
                          sm_sub_name=s.subrepo_name,
                          sm_sub_vers=s.subrepo_vers)
            for e in rinfo.get('submodules', list())
            for s in e.gitmodules_repovers]
    }

def duplicate_info_for_repo(results, rinfo, rs, repo_url_map, repo_name_map):
    results['branches'].extend([
        BranchRef(reponame=rs.repo_name, branchname=e.branchname, branchref=e.branchref)
        for e in rinfo['branches']])
    results['submodules'].extend([
        SubModuleInfo(sm_repo_name=rs.repo_name,
                      sm_branch=e.sm_branch,
                      sm_pullreq_id=e.sm_pullreq_id,
                      sm_sub_name=rn,
                      sm_sub_vers=e.sm_sub_vers)
        for e in rinfo['submodules']
        for rn in repo_url_map[repo_name_map[e.sm_sub_name]]])  # KWQ: this is horrible: need difference between package and repo (N:1)
    results['pullreqs'].extend([
        PRInfo(pr_target_repo=rs.repo_name,
               pr_srcrepo_url=e.pr_srcrepo_url,
               pr_branch=e.pr_branch,
               pr_revision=e.pr_revision,
               pr_ident=e.pr_ident,
               pr_status=e.pr_status,
               pr_title=e.pr_title,
               pr_user=e.pr_user,
               pr_email=e.pr_email)
        for e in rinfo['pullreqs']])
    results['subrepos'].union(rinfo['subrepos'])


class GetVCSInfo(object):
    def __init__(self):
        self.rdict = {}  # key = repo_url, value = dict of get_repo_info() results

    def get_vcs_info(self, input_desc):
        "Get information about the set of repositories described by input_desc"

        # Uses any cached information in self.rdict

        all_repos = AllRepos(input_desc.RL)

        for repo in input_desc.RL:
            if repo.repo_url not in self.rdict:
                self.rdict[repo.repo_url] = get_repo_info(repo, input_desc, all_repos)

        # Discovered repos might have URL's already seen and don't need to be
        # re-probed.  An example: a submodule that contains several explicit
        # packages will have been described in RL for those packages, but
        # exist as a single entry in the discovered submodules.

        for repo in all_repos.discovered_repos():
            if repo.repo_url not in self.rdict:
                self.rdict[repo.repo_url] = get_repo_info(repo, input_desc, all_repos)

        repo_url_map = defaultdict(list)
        repo_name_map = dict()
        for each in all_repos.all_repos():
            repo_url_map[each.repo_url].append(each.repo_name)
            repo_name_map[each.repo_name] = each.repo_url

        r = {'branches':[], 'pullreqs':[], 'submodules':[],
             'subrepos':set([e for e in input_desc.RL if not e.project_repo])
        }
        for repo in all_repos.all_repos():
            duplicate_info_for_repo(r, self.rdict[repo.repo_url], repo, repo_url_map, repo_name_map)
        return r
