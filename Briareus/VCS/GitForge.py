import attr
import base64
import configparser
import datetime
import json
import os
import requests
import logging
from urllib.parse import urlparse, urlunparse
from Briareus.VCS.InternalMessages import *
from .ForgeAccess import *
from typing import (Any, Dict, NoReturn, Optional, Tuple, Type, TypeVar, Union)

LocalCachePeriod = datetime.timedelta(minutes=1, seconds=35)


ResponseTy = Union[int,   # http error code, usually NotFound
                   Dict[str,Any],   # if JSON, parsed into a python dict, OR
                   List[Any],       # if JSON, parsed into a list
                   str,   # if not JSON, return the text
]


class RemoteGit__Info(object):
    """Common functionality for remote Git retrieval (Github or Gitlab)."""
    def __init__(self, api_url: str) -> None:
        self._url = api_url
        self._request_session = requests.Session()
        self._rsp_cache: Dict[str, Union[int, requests.Response]] = {}   # requrl:response
        self._rsp_fetched: Dict[str, datetime.datetime] = {}  # requrl:fetch_time
        self._get_count = 0
        self._req_count = 0
        self._refresh_count = 0

    NotFound = 404

    def stats(self) -> Dict[str, Any]:
        return { "url": self._url,
                 "rsp_cache_keys": list(self._rsp_cache.keys()),
                 "get_info_reqs": self._get_count,
                 "remote_reqs": self._req_count,
                 "remote_refreshes": self._refresh_count
                 # n.b. get_info_reqs - remote_reqs - len(rsp_cache_keys) = error or 404 responses
        }

    trailer = '.git'
    trailer_len = len(trailer)

    @staticmethod
    def repo_url(self, url: str) -> str:
        "Drops any additional path elements beyond the first two: owner and repo"
        parsed = urlparse(url)
        return urlunparse(
            parsed._replace(path = '/'.join(parsed.path.split('/')[:3]) ))

    def api_req(self, reqtype: str, notFoundOK=False, raw=False) -> ResponseTy:
        self._get_count += 1
        if reqtype.startswith('//'):
            # Drop the owner/repo at the tail of the url
            parsed = urlparse(self._url)
            req_url = urlunparse(parsed._replace(path='/'.join(parsed.path.split('/')[:-2] + [reqtype[2:]])))
        else:
            req_url = self._url + reqtype
        return self._get_cached_links_pageable_url(req_url, notFoundOK=notFoundOK, raw=raw)

    def _get_cached_links_pageable_url(self, req_url: str,
                                       notFoundOK: bool,
                                       raw: bool) -> ResponseTy:
        rsp = self._get_cached_url(req_url, notFoundOK=notFoundOK, raw=raw)
        if isinstance(rsp, int):
            return rsp
        if 'Link' not in rsp.headers or not rsp.links.get('next', None):
            return rsp.text if raw else rsp.json()
        # $ curl -v https://api.github.com/repos/matterhorn-chat/matterhorn/branches
        # ...
        # Link: <https://api.github.com/repositories/66096261/branches?page=2>; rel="next", \
        #       <https://api.github.com/repositories/66096261/branches?page=2>; rel="last"
        # ...
        #
        # [Line continuation added for readability]
        #
        # Used by both github and gitlab APIs.  Supported easily by
        # requests
        # https://2.python-requests.org/en/master/user/advanced/#link-headers
        nextrsp = self._get_cached_links_pageable_url(rsp.links['next']['url'],
                                                      notFoundOK=notFoundOK, raw=raw)
        if isinstance(nextrsp, int):
            return nextrsp
        if raw:
            assert isinstance(nextrsp, str)
            return rsp.text + nextrsp
        if isinstance(nextrsp, dict):
            nextrsp.update(rsp.json())
            return nextrsp
        assert isinstance(nextrsp, list)
        return rsp.json() + nextrsp

    def _get_cached_url(self, req_url: str,
                        notFoundOK: bool,
                        raw: bool) -> Union[int, requests.Response]:
        last_one = self._rsp_cache.get(req_url, None)
        if last_one:
            # If fetched within the local cache period, just re-use
            # the same response
            last = self._rsp_fetched.get(req_url, None)
            if last:
                if datetime.datetime.now() - last < LocalCachePeriod:
                    return self._rsp_cache[req_url]
        # If already fetched, pass the header tags to the server in
        # the request so that the server can respond with either a 304
        # "Not Modified" or the new data (the 304 does not count
        # against the server's rate limit).
        hdrs = {}
        if last_one and last_one != self.NotFound and \
           isinstance(last_one, requests.Response):
            if 'ETag' in last_one.headers:
                hdrs = { "If-None-Match": last_one.headers['ETag'] }
            elif last_one and 'Last-Modified' in last_one.headers:
                hdrs = { "If-Modified-Since": last_one.headers['Last-Modified'] }
        self._req_count += 1
        rsp = self._request_session.get(req_url, headers = hdrs)
        if rsp.status_code == 304:  # Not Modified
            self._refresh_count += 1
            self._rsp_fetched[req_url] = datetime.datetime.now()
            return self._rsp_cache[req_url]
        elif rsp.status_code == 200:
            self._rsp_cache[req_url] = rsp
            self._rsp_fetched[req_url] = datetime.datetime.now()
        elif rsp.status_code == 404 and notFoundOK:
            self._rsp_cache[req_url] = self.NotFound
            self._rsp_fetched[req_url] = datetime.datetime.now()
            return self.NotFound
        else:
            try:
                for err in rsp.json().get('errors', []):
                    logging.error('GET %s err: %s', req_url,
                                  err.get('message', rsp.status_code))
            except json.decoder.JSONDecodeError:
                logging.error('GET %s failed: %s', req_url, rsp.text)
            rsp.raise_for_status()
        return rsp

    def api_post(self, reqtype: str,
                 data: object,
                 notFoundOK=False) -> Union[int, requests.Response]:
        # POST operations are not cached
        req_url = self._url + reqtype
        rsp = self._request_session.post(req_url, json=data)
        if notFoundOK and rsp.status_code == 404:
            return self.NotFound
        for err in rsp.json().get('errors', []):
            logging.error('POST %s err: %s', req_url,
                         err.get('message', rsp.status_code))
        rsp.raise_for_status()
        return rsp

    def get_file_contents_raw(self, target_filepath: str, branch: str) -> Union[int,str]:
        rsp = self._get_file_contents_info(target_filepath, branch)
        if isinstance(rsp, type(self.NotFound)):
            return rsp
        if isinstance(rsp, dict):
            if rsp['encoding'] != 'base64':
                logging.error('Unknown encoding for %s, branch %s, repo %s: %s',
                              target_filepath, branch, self._url)
                return self.NotFound
            return base64.b64decode(rsp['content']).decode('utf-8')
        else:
            assert isinstance(rsp, str)
        return rsp

    def _get_file_contents_info(self, target_filepath: str, branch: str) -> ResponseTy:
        raise NotImplementedError('Subclass %s must define _get_file_contents_info!' %
                                  str(self.__class__.__name__))

    def get_gitmodules(self, reponame: str,
                       branch: str,
                       pullreq_id: Optional[str]) -> GitmodulesRepoVers:
        rsp = self.get_file_contents_raw('.gitmodules', branch)
        if isinstance(rsp, int):
            if rsp == self.NotFound:
                return GitmodulesRepoVers(reponame, branch, pullreq_id, [])
            else:
                raise RuntimeError('Got unexpected response to reading .gitmodules: %s' %
                                   rsp)
        return self.parse_gitmodules_contents(reponame, branch, pullreq_id, rsp)

    def parse_gitmodules_contents(self, reponame:str,
                                  branch: str,
                                  pullreq_id: Optional[str],
                                  gitmodules_contents: str) -> GitmodulesRepoVers:
        gitmod_cfg = configparser.ConfigParser()
        gitmod_cfg.read_string(gitmodules_contents)
        ret = []
        for remote in gitmod_cfg.sections():
            # Note: if the URL of a repo moves, need a new name for the moved location?  Or choose not to track these changes?
            submod_info = self._get_file_contents_info(gitmod_cfg[remote]['path'], branch)
            if isinstance(submod_info, int):
                if submod_info != self.NotFound:
                    raise RuntimeError('Unexpected response getting submod ' +
                                       gitmod_cfg[remote]['path'] + ' revision ' + branch +
                                       ': ' + str(submod_info))
                # Is the repo in .gitmodules valid?
                valid_repo = self.api_req('', notFoundOK=True)
                if valid_repo == self.NotFound:
                    logging.warning('Invalid URL for submodule %s, %s: using "%s"',
                                    remote, self._url, os.path.split(gitmod_cfg[remote]['path'])[-1])
                    # The submodule added to .gitmodules specified an
                    # invalid repository.  Have to assume the name is
                    # the last component of the path.
                    ret.append(SubRepoVers(os.path.split(gitmod_cfg[remote]['path'])[-1],
                                           'invalid_remote_repo',
                                           'unknownRemoteRefForPullReq'))
                else:
                    # The submodule was added to .gitmodules, but no
                    # actual version of the remote repo was committed, so
                    # no reference SHA can be known.  Instead, use a
                    # reference sha that is completely invalid, which
                    # should cause a build failure, as long as the remote
                    # URL itself seems to be valid.
                    logging.warning('in %s branch %s, there is no submodule commit for .gitmodule'
                                    ' path %s, url %s',
                                    self._url, branch,
                                    gitmod_cfg[remote]['path'],
                                    gitmod_cfg[remote]['url'])
                    # Generate an invalid revision that will cause this build to fail on fetch of source
                    ret.append(SubRepoVers(gitmod_cfg[remote]['path'].split('/')[-1],
                                           gitmod_cfg[remote]['url'],
                                           'unknownRemoteRefForPullReq'))
            else:
                assert not isinstance(submod_info, list)
                srv = self._subrepo_version(remote, gitmod_cfg[remote], submod_info)
                if srv:
                    ret.append(srv)
        return GitmodulesRepoVers(reponame, branch, pullreq_id, ret)

    def _subrepo_version(self,
                         remote_name: str,
                         remote_info: configparser.SectionProxy,
                         remote_vers: Union[str, Dict[str,Any]]) -> Optional[SubRepoVers]:
        raise NotImplementedError('Must subclass %s._subrepo_version for forge type.' %
                                  self.__class__.__name__)


# ----------------------------------------------------------------------
#
# Gitlab access
#
# Examples:
#
# $ CURL="curl -H 'Content-type: application/json' -H 'Private-token: TOKVAL' -L -v"
# $ ${CURL} https://gitlab.mycompany.com/api/v4/groups
# $ ${CURL} https://gitlab.mycompany.com/api/v4/groups/239
# $ ${CURL} https://gitlab.mycompany.com/api/v4/projects
# $ ${CURL} https://gitlab.mycompany.com/api/v4/projects/32
#     look at _links in response for other options
# $ ${CURL} https://gitlab.mycompany.com/api/v4/projects/32/merge_requests


class GitLabInfo(RemoteGit__Info):
    """Retrieve information from gitlab via API with cacheing.  Note that
       this object does not maintain a "name" for the repo because
       several projects may share the same repo.
    """
    def __init__(self, repo_api_location: RepoAPI_Location) -> None:
        super(GitLabInfo, self).__init__(self.get_api_url(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.headers.update({'Private-Token': repo_api_location.apitoken})

    def get_api_url(self, url: str) -> str:
        parsed = urlparse(url)
        return urlunparse(
            parsed._replace(path = 'api/v4/projects/' + parsed.path[1:].replace('/', '%2F')))


    def _src_repo_url(self, mergereq: Dict[str, Any]) -> Union[str, int, Tuple[str, str]]:
        if 'source_project_url' in mergereq:
            return mergereq['source_project_url']
        # It's a source_project_id, but since it's on this gitlab
        # forge, it's in this repo as a local branch.  The proper URL
        # is not known here, only the forge API url, so defer the
        # actual URL to the caller who does have that information.
        if mergereq.get('source_project_id', "no_spid") == \
           mergereq.get('target_project_id', "no_tpid"):
            return "SameProject"
        rsp = self.api_req('//projects/%d' % mergereq.get('source_project_id', "no_spid"),
                           notFoundOK=True)
        if isinstance(rsp, int):
            return rsp
        assert isinstance(rsp, dict)
        return ("DifferentProject", rsp['name'])

    def get_pullreqs(self, reponame: str) -> PullReqsData:
        rsp = self.api_req('/merge_requests?scope=all&state=all')
        # Gather {"upvotes": 0, "downvotes": 0, "approvals_before_merge": 0} for analysis phase
        # Use {"work_in_progress": true} to ignore the PR
        # Use {"merge_status": "can_be_merged"} for analysis phase?

        # n.b. GitLab pullreqs have an id and and iid.  The iid is the
        # one that is presented to the user on the Web page.
        assert isinstance(rsp, list)

        preqs = []
        for pr in rsp:
            src_repo_url = self._src_repo_url(pr)
            src_branch = pr['source_branch']
            if src_repo_url == self.NotFound:
                # Gitlab has a dubious feature where if a user
                # forks a private repository, the user's fork is
                # NOT accessible to other members of the private
                # repository.  When a merge request is generated
                # from that private repository, the merge request
                # information is visible as part of the merge
                # request, but the source repository is *not*
                # visible.  For Briareus and the buildsys, this
                # means that the .gitmodules of the source
                # repository cannot be examined (if it was a
                # project repository) and that the buildsys cannot
                # check out the source repo to build against.
                #
                # Per gitlab.com/gitlab-org/gitlab/-/issues/38216, the
                # "refs/merge-requests/:iid/head" in the target repo
                # represents the accessible merge source.

                # logging.warning('Inaccessible source repo for gitlab repo %s PR #%d "%s"; ignoring',
                #                 reponame, pr['iid'], pr['title'])

                src_repo_url = "SameProject"
                src_branch = 'refs/merge-requests/' + str(pr["iid"]) + '/head'

            assert isinstance(src_repo_url, str)

            prinfo = PullReqInfo(str(pr["iid"]),   # for user reference
                                 pullreq_status = { "closed": PRSts_Closed,
                                                    "merged": PRSts_Merged,
                                                    "opened": PRSts_Active,
                                                    "locked": PRSts_Active,  # short-lived transitional
                                 }.get(pr["state"], PRSts_Closed)(),
                                 pullreq_title=pr["title"],    # for user reference
                                 pullreq_srcurl=src_repo_url,
                                 pullreq_branch=src_branch,
                                 pullreq_ref=pr["sha"],
                                 pullreq_user=pr['author']['username'],
                                 pullreq_email=self.get_user_email(str(pr['author']['id'])),
                                 pullreq_mergeref=None)
            preqs.append(prinfo)

        return PullReqsData(reponame, preqs)

    def get_user_email(self, userid: str) -> str:
        userinfo = self.api_req('//users/' + userid, notFoundOK=True)
        if userinfo == self.NotFound:
            return ''
        assert isinstance(userinfo, dict)
        return userinfo['public_email']

    def get_branches(self) -> List[Dict[str,str]]:
        r = self.api_req('/repository/branches')
        assert isinstance(r, list)
        return [ dict([('name', e['name']),
                       ('ref', e['commit']['id']),
                      ])
                 for e in r ]

    def _get_file_contents_info(self, target_filepath: str, branch: str) -> ResponseTy:
        return self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '?ref=' + branch)

    def get_file_contents_raw(self, target_filepath: str, branch: str) -> Union[int,str]:
        rsp = self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '/raw?ref=' + branch,
                            # Sometimes this cannot be accessed, and
                            # the higher levels handle this.  This
                            # will frequently happen when someone
                            # forks a Private repo where a PAT has
                            # been added for the Private repo but the
                            # user's fork doesn't propagate the PAT.
                            notFoundOK=True,
                            raw=True)
        assert isinstance(rsp, (int,str))
        return rsp

    def _subrepo_version(self, remote_name: str,
                         remote_info: configparser.SectionProxy,
                         submod_info: Union[str, Dict[str,Any]]) -> Optional[SubRepoVers]:
        if isinstance(submod_info, dict):
            return SubRepoVers(submod_info['file_name'],
                               remote_info['url'],
                               submod_info['blob_id'])
        else:
            raise RuntimeError('Gitlab._subrepoversion submod_info should be str'
                               ', got %s :: %s' % (str(submod_info), type(submod_info)))

    def set_commit_status(self, sts: str,
                          desc: str,
                          commitref: str,
                          url: str = '',
                          context_ref: str = '') -> Union[int, requests.Response]:
        """Posts a status to the commit (usually shown on the pull request/
           merge request page.  The sts should be one of "pending",
           "running", "success", "canceled", or "failed".  The desc is
           a short description (about 144 chars), the commitref is the
           sha256 commit the status should be posted about.  The
           optional url is a "click for details" link for the status.
           The context_ref is used to make this status report unique
           (github uses a "context" as the index key for a status:
           posting to the same context overrides previous status
           values).

        """

        # Note: github uses "failure" but gitlab uses "failed".

        # Note: gitlab has a "pending" status, but it is not used here
        # because while in "pending" it rejects (400 Client Error) any
        # attempts to set any state other than "running".  However,
        # any transition between running, failed, and success is
        # allowed (and once in this set of states, an attempt to set
        # "pending" sets the actual state to "running", not
        # "pending").

        return self.api_post('/statuses/' + commitref,
                             {"state": ("failed" if sts == "failure" else
                                        "running" if sts == "pending" else sts),
                              "description": desc,
                              "target_url": url,
                              "context": ("ci/briareus:" + context_ref
                                          if context_ref else "ci/briareus"),
                             },
                             # Allow 404: means no permissions to set a status, so don't retry
                             notFoundOK=True)


# ----------------------------------------------------------------------
#
# Github access
#
# Examples:
#
# $ CURL="curl -H 'Content-type: application/json' -H 'Private-token: TOKVAL' -L -v"
# $ ${CURL} https://api.github.com/repos/kquick/test
# $ ${CURL} https://api.github.com/repos/kquick/test/pulls


class GitHubInfo(RemoteGit__Info):
    """Retrieve information from github via API with cacheing.  Note that
       this object does not maintain a "name" for the repo because
       several projects may share the same repo.
    """
    def __init__(self, repo_api_location: RepoAPI_Location):
        super(GitHubInfo, self).__init__(self.get_api_url(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.auth = requests.auth.HTTPBasicAuth(
                *tuple(repo_api_location.apitoken.split(':')))

    def get_api_url(self, url: str) -> str:
        """Converts a remote repository URL into a form that is useable for
           the Github API (https://developer.github.com/v3) to allow
           API-related requests.
        """
        parsed = urlparse(url)
        if parsed.netloc == 'github.com':
            return urlunparse(
                parsed._replace(netloc = 'api.github.com',
                                path = 'repos' + parsed.path))
        raise RuntimeError("No API URL parsing for: %s [ %s ]" % (url, str(parsed)))

    def get_pullreqs(self, reponame: str) -> PullReqsData:
        # Need closed PR's as well as open: when a PR is closed or
        # merged, master should be used instead of that branch in
        # conjunction with identical branch open PR's in other repos.
        rsp = self.api_req('/pulls?state=all')
        assert isinstance(rsp, list)
        # May want to echo either ["number"] or ["title"]
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        # ["head"]["repo"]["url"] is the github repo url for the source repo of the PR
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        preqs = [ PullReqInfo(str(pr["number"]),   # for user reference
                              pullreq_status=(PRSts_Merged() if pr["merged_at"]
                                              else (PRSts_Closed() if pr["state"] == "closed"
                                                    else PRSts_Active())),
                              pullreq_title=pr["title"],    # for user reference
                              pullreq_srcurl=(lambda r: r["html_url"] if r else '')(
                                  pr["head"]["repo"]),  # source repo URL might be gone if old
                              pullreq_branch=pr["head"]["ref"],          # source repo branch
                              pullreq_ref=pr["head"]["sha"],         # for github, can also use branch ^
                              pullreq_user=pr["user"]["login"],
                              pullreq_email=self.get_user_email(pr["user"]["login"]),
                              pullreq_mergeref=pr["merge_commit_sha"])
                  for pr in rsp ]
        return PullReqsData(reponame, preqs)

    def get_user_email(self, username: str) -> str:
        userinfo = self.api_req('//users/' + username, notFoundOK=True)
        if userinfo == self.NotFound:
            return ''
        assert isinstance(userinfo, dict)
        return userinfo['email'] or ''

    def get_branches(self) -> List[Dict[str,str]]:
        r = self.api_req('/branches')
        assert isinstance(r, list)
        return [ dict([('name', e['name']),
                       ('ref', e['commit']['sha']),
                      ])
                 for e in r ]

    def _get_file_contents_info(self, target_filepath: str, branch: str) -> ResponseTy:
        return self.api_req('/contents/' + target_filepath + '?ref=' + branch, notFoundOK=True)

    def _subrepo_version(self, remote_name: str,
                         remote_info: configparser.SectionProxy,
                         submod_info: Union[str, Dict[str,Any]]) -> Optional[SubRepoVers]:
        if not isinstance(submod_info, dict):
            raise RuntimeError('GitHubInfo._subrepo_version must get dict submod_info'
                               ', not: %s :: %s' % (str(submod_info), type(submod_info)))
        if submod_info['type'] != 'submodule':
            logging.warning('Found %s at %s, but expected a submodule',
                            submod_info['type'], remote_name)
            return None # ignore this submodule entry
        return SubRepoVers(submod_info['name'],
                           submod_info['submodule_git_url'],
                           submod_info['sha'])

    def set_commit_status(self, sts: str,
                          desc: str,
                          commitref: str,
                          url: str = '',
                          context_ref: str = '') -> Union[int, requests.Response]:
        """Posts a status to the commit (usually shown on the pull request/
           merge request page.  The sts should be one of "pending",
           "success", or "failure".  The desc is a short description
           (about 144 chars), the commitref is the sha256 commit the
           status should be posted about.  The optional url is a
           "click for details" link for the status.  The context_ref
           is used to make this status report unique (github uses a
           "context" as the index key for a status: posting to the
           same context overrides previous status values).
        """

        return self.api_post('/statuses/' + commitref,
                             {"state": sts,
                              "description": desc,
                              "target_url": url,
                              "context": ("ci/briareus:" + context_ref
                                          if context_ref else "ci/briareus"),
                             },
                             # Allow 404: means no permissions to set a status, so don't retry
                             notFoundOK=True)
