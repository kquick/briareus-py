"""Implementation of Action posting a Status to a VCS Forge
(e.g. Github, Gitlab, etc).

Only enabled if BRIAREUS_FORGE_STATUS is set; the value is "1" for
fully enabling github status, or else a comma-separated list of branch
names for which status can be posted.

"""

import os
import platform
import sys
import requests
from collections import defaultdict
from thespian.actors import *
from Briareus.Actions.Content import gen_content
from Briareus.Types import SetForgeStatus, PRCfg
from Briareus.VCS.GitForge import to_http_url, RepoAPI_Location, GitHubInfo, GitLabInfo


def do_set_forge_status(action, full_report, runctxt):
    "Posts a status update to the VCS Forge (github, gitlab, etc.)"
    project = action.notification.subject
    rec = action.targetrepos
    done = action.updated
    post_to = set(rec) - set(done)
    if post_to:
        _, desc = gen_content('forge_status', action.notification, runctxt)
        if desc:
            posted_to = set_forge_status(post_to,
                                         action.notification.what,
                                         desc,
                                         runctxt,
                                         project,
                                         action.notification.params)
        else:
            # Output is suppressed, so indicate all targets have been satisfied
            posted_to = post_to
        action = SetForgeStatus(targetrepos=action.targetrepos,
                                notification=action.notification,
                                updated = list(set(action.updated).union(posted_to)))
    return action


def set_forge_status(forge_list, status, desc, runctxt, project, notify_params):
    can_post = os.getenv('BRIAREUS_FORGE_STATUS', None)
    if not can_post:
        print('Warning: post to %s suppressed: %s (for %s)'
              % (forge_list, desc, notify_params.prtype),
              file=sys.stderr)
        return forge_list


    proj_results = [ each
                     for each in runctxt.result_sets
                     if project == each.inp_desc.PNAME ][0] # must match at least one!

    url_and_rev = defaultdict(list)  # key=(loc, rev), value = [repos]

    for r in proj_results.inp_desc.RL:  # r is Description.RepoDesc
        if r.repo_name in forge_list:
            repo, loc, rev = get_repo_loc_and_PR_rev(r, proj_results, notify_params)
            url_and_rev[(loc,rev)].append(repo)

    sts = { "pr_projstatus_pending": "pending",
            "pr_projstatus_good": "success",
            "pr_projstatus_fail": "failure"
    }[status]

    stsurl = proj_results.builder.get_project_url(project)

    print('Forge post "', desc, '" [',sts,'] about ',
          notify_params.prtype,'to', url_and_rev,'and url',stsurl)

    successful = []
    for loc,rev in url_and_rev:
        forge = GitForge(loc)
        try:
            if forge.set_commit_status(sts, desc, rev, stsurl, project):
                successful.extend(url_and_rev[(loc,rev)])
        except Exception as ex:
            print('Error posting forge status to %s: %s'
                  % (str(loc), str(ex)),
                  file = sys.stderr)
            successful.extend(url_and_rev[(loc,rev)])

    return successful


def get_repo_loc_and_PR_rev(r, proj_results, notify_params):
    tgtloc = to_http_url(r.repo_url, proj_results.inp_desc.RX)
    for p in notify_params.prcfg:  # p is Types.PRCfg
        if isinstance(p, PRCfg) and p.reponame == r.repo_name:
            for pr in proj_results.repo_info['pullreqs']: # pr is VCS.InternalMessages.PRInfo
                if pr.pr_target_repo == r.repo_name \
                   and pr.pr_branch == p.branch \
                   and pr.pr_ident == p.pr_id:
                    return r.repo_name, tgtloc, pr.pr_revision
    return None, None, None


class GitForge(object):
    def __init__(self, repoloc):
        self._repoloc = repoloc  # RepoAPI_Location
        self._ghinfo = (GitHubInfo(repoloc) if 'github' in repoloc.apiloc else
                        (GitLabInfo(repoloc) if 'gitlab' in repoloc.apiloc else None))
        if not self._ghinfo:
            raise ValueError('Cannot determine type of remote repo at %s'
                             % self.repospec.repo_api_loc.apiloc)


    def set_commit_status(self, sts, desc, commitref, url='', context_ref=''):
        rval = self._ghinfo.set_commit_status(sts, desc, commitref, url, context_ref)
        if rval == self._ghinfo.NotFound or rval.status_code in [ 201, 404 ]:
            return True
        print('set_commit_sts response',rval)
        return False
