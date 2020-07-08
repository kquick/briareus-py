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
from datetime import timedelta
from thespian.actors import ActorSystem
from Briareus.Actions.Actors.Msgs import *
from Briareus.Actions.Content import gen_content
from Briareus.Input.Description import RepoDesc
from Briareus.Types import SetForgeStatus, PRCfg, PRData, RunContext, ResultSet
from Briareus.VCS.ForgeAccess import to_http_url
from typing import Any, Dict, Optional, Set, Tuple


SET_FORGE_STATUS_TIMEOUT = timedelta(seconds=20)


def do_set_forge_status(action: SetForgeStatus,
                        full_report: str,
                        runctxt: RunContext,
                        report_supplement: Dict[str,Any]) -> SetForgeStatus:
    "Posts a status update to the VCS Forge (github, gitlab, etc.)"
    project = UserURL(action.notification.subject)
    rec = action.targetrepos
    done = action.updated
    post_to = set(rec) - set(done)
    if post_to:
        _, desc = gen_content('forge_status', action.notification, runctxt)
        if desc:
            assert isinstance(action.notification.params, PRData)
            posted_to = set_forge_status(post_to,
                                         action.notification.what,
                                         desc,
                                         runctxt,
                                         report_supplement,
                                         project,
                                         action.notification.params)
        else:
            # Output is suppressed, so indicate all targets have been satisfied
            posted_to = post_to
        action = SetForgeStatus(targetrepos=action.targetrepos,
                                notification=action.notification,
                                updated = list(set(action.updated).union(posted_to)))
    return action


def set_forge_status(forge_list: Set[str],  # target repos
                     status: str,
                     desc: str,
                     runctxt: RunContext,
                     report_supplement: Dict[str,Any],
                     project: UserURL,
                     notify_params: PRData) -> Set[str]:
    if not runctxt.actor_system:
        runctxt.actor_system = ActorSystem('multiprocTCPBase')
    asys = runctxt.actor_system

    proj_results = [ each
                     for each in runctxt.result_sets
                     if each.inp_desc and project == each.inp_desc.PNAME ][0]
    # must match at least one!

    # Modules may share the same actual repository, so the status need
    # only be set once for the entire repository.  Build a translation
    # dictionary from url locations and revision to the list of
    # modules.

    url_and_rev = defaultdict(list)  # key=(loc, rev), value = [repos]

    for r in (list(proj_results.inp_desc.RL
                   if proj_results.inp_desc else []) +     # r is Description.RepoDesc
              list(proj_results.repo_info['subrepos'])):
        if r.repo_name in forge_list:
            rlinfo = get_repo_loc_and_PR_rev(r, proj_results, notify_params)
            if rlinfo is not None:
                repo, loc, rev = rlinfo
                url_and_rev[(loc,rev)].append(repo)

    # Determine the status to set, and the details URL to post with each status

    sts = { "pr_projstatus_pending": "pending",
            "pr_projstatus_good": "success",
            "pr_projstatus_fail": "failure"
    }[status]

    dflt_url_spec = report_supplement.get('status_url', None)
    user_url_spec = (proj_results.inp_desc.REP.get('status_url', dflt_url_spec)
                     if proj_results.inp_desc
                     else dflt_url_spec)
    stsurl = (
        user_url_spec.format(project=project)
        if user_url_spec else
        (proj_results.builder.get_project_url(project)
         if proj_results.builder
         else '-none-')
    )

    print('Forge post "', desc, '" [',sts,'] about ',
          notify_params.prtype,'to', url_and_rev,'and url',stsurl)

    # Now pass the details to the actor troupe that will actually set
    # the status.  The actors will set the status concurrently to
    # accomodate slow forge responses or errors.  Use a global name
    # for this actor to re-connect to the existing "daemon"
    rsp = asys.ask(asys.createActor('Briareus.Actions.Actors.SetForgeStatus.SetForgeStatus',
                                    globalName='SetForgeStatus'),
                   toJSON(
                       NewForgeStatus(
                           # n.b. cannot pass a tuple key to the Actor
                           # because JSON doesn't support tuples, so
                           # tuples get converted to lists and lists
                           # cannot be used as dict keys.  Instead,
                           # translate the url_and_rev dict into a
                           # list of RepoURLRevProjURL objects, and
                           # allow the Actor to reconstitute as needed
                           # on the other end.
                           [ RepoURLRevProjURL(ur[0], ur[1], url_and_rev[ur])
                             for ur in url_and_rev ],
                           sts, desc, stsurl, project)
                   ),
                   SET_FORGE_STATUS_TIMEOUT)
    if rsp == None:
        raise RuntimeError('Timeout waiting for SetForgeStatus response')

    rspobj = fromJSON(rsp)
    if isinstance(rspobj, Posted):
        return set(rspobj.successful)
    raise RuntimeError('Unexpected response to SetForgeStatus request: %s' % str(rsp))


def get_repo_loc_and_PR_rev(r: RepoDesc,
                            proj_results: ResultSet,
                            notify_params: PRData) -> Optional[Tuple[str,
                                                                     RepoAPI_Location,
                                                                     str]]:
    if proj_results.inp_desc is None:
        return None
    tgtloc = to_http_url(r.repo_url, proj_results.inp_desc.RX)
    for p in notify_params.prcfg:  # p is Types.PRCfg
        if isinstance(p, PRCfg) and p.reponame == r.repo_name:
            return r.repo_name, tgtloc, p.revision
    return None
