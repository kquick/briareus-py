# Interaction with a Git repository

import os
import logging
import hashlib
import subprocess
import requests
import json
import base64
import configparser
from urllib.parse import urlparse, urlunparse
from thespian.actors import *
from thespian.initmsgs import initializing_messages
from Briareus.VCS.InternalMessages import *
import datetime

LocalCachePeriod = datetime.timedelta(minutes=1, seconds=35)


@initializing_messages([('repospec', RepoRemoteSpec)], init_passthru=True)
class GitRepoInfo(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitRepoInfo, self).__init__(*args, **kw)
        self._ghinfo = None
        self.repospec = RepoRemoteSpec("no-url", None, None)
        self.repo_cachedir = "no cachedir"

    def receiveMsg_RepoRemoteSpec(self, msg, sender):
        self.repo_cachedir = "-<none>-"
        self._ghinfo = (GitHubInfo(self.repospec.repourl, request_auth=msg.request_auth)
                        if 'github' in self.repospec.repourl else
                        (GitLabInfo(self.repospec.repourl, request_auth=msg.request_auth)
                         if 'gitlab' in self.repospec.repourl else
                         None))
        if not self._ghinfo:
            raise ValueError('Cannot determine type of remote repo at %s'
                             % self.repospec.repourl)

    def receiveMsg_GetPullReqs(self, msg, sender):
        try:
            rsp = self._ghinfo.get_pullreqs(msg.reponame)
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  'GetPullReqs - ' + str(err)))
        else:
            self.send(msg.orig_sender, rsp)

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        try:
            rsp = self._ghinfo.get_branches()
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  'HasBranch - ' + str(err)))
        else:
            blist = [ b['name'] for b in rsp ]
            chk = branch in blist
            self.send(msg.orig_sender, BranchPresent(msg.reponame, branch, chk,
                                                     known_branches=blist))


    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        try:
            rval = self._ghinfo.get_gitmodules(msg.reponame, branch,
                                               repo_src_url=msg.alt_repo_url)
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  'GitmodulesData - ' + str(err)))
        else:
            self.send(msg.orig_sender, rval)


    def receiveMsg_str(self, msg, sender):
        if msg == "status":
            self.send(sender, self._ghinfo.stats() if self._ghinfo else
                      { "url": str(self.repospec.repourl) + " (never accessed)",
                      })


class RemoteGit__Info(object):
    """Common functionality for remote Git retrieval (Github or Gitlab)."""
    def __init__(self, api_url):
        self._url = api_url
        self._request_session = requests.Session()
        self._rsp_cache = {}
        self._rsp_fetched = {}
        self._get_count = 0
        self._req_count = 0
        self._refresh_count = 0

    NotFound = 404

    def stats(self):
        return { "url": self._url,
                 "rsp_cache_keys": list(self._rsp_cache.keys()),
                 "get_info_reqs": self._get_count,
                 "remote_reqs": self._req_count,
                 "remote_refreshes": self._refresh_count
                 # n.b. get_info_reqs - remote_reqs - len(rsp_cache_keys) = error or 404 responses
        }

    trailer = '.git'
    trailer_len = len(trailer)

    def _remove_trailer(self, path):
        return (path[:-self.trailer_len]
                if path[-self.trailer_len:] == self.trailer
                else path)

    def to_http_url(self, url):
        # Convert "git@foo.com:group/proj" to "https://foo.com/group/proj"
        if url.startswith("git@"):
            trimmed_url = self._remove_trailer(url[len('git@'):])
            spl = trimmed_url.split(':')
            return self.to_http_url('https://%s/%s' % (spl[0], ':'.join(spl[1:])))
        return self._remove_trailer(url)

    @staticmethod
    def repo_url(self, url):
        "Drops any additional path elements beyond the first two: owner and repo"
        parsed = urlparse(url)
        return urlunparse(
            parse._replace(path = '/'.join(parse.path.split('/')[:3]) ))

    def api_req(self, reqtype, repo_url_override=None, notFoundOK=False, raw=False):
        # n.b. repo_url_override is usually used for PullRequests,
        # which exist in the target repo but reference a branch in a
        # source repo, so the repo_url_override specifies the latter.
        # There is an assumption that PullRequests are for the same
        # forge (e.g. Github or gitlab).
        self._get_count += 1
        req_url = (repo_url_override or self._url) + reqtype
        return self._get_cached_links_pageable_url(req_url, notFoundOK=notFoundOK, raw=raw)

    def _get_cached_links_pageable_url(self, req_url, notFoundOK, raw):
        rsp = self._get_cached_url(req_url, notFoundOK=notFoundOK, raw=raw)
        if rsp == self.NotFound:
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
        if raw:
            return rsp.text + nextrsp
        if isinstance(nextrsp, dict):
            nextrsp.update(rsp.json())
            return nextrsp
        elif isinstance(nextrsp, list):
            return rsp.json() + nextrsp
        else:
            logging.error('Unable to join nextrsp type %s to this response type %s',
                          type(nextrsp), type(rsp.json()))

    def _get_cached_url(self, req_url, notFoundOK, raw):
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
        if last_one and last_one != self.NotFound:
            if 'ETag' in last_one.headers:
                hdrs = { "If-None-Match": last_one.headers['ETag'] }
            elif last_one and 'Last-Modified' in last_one.headers:
                hdrs = { "If-Modified-Since": last_one.headers['Last-Modified'] }
        self._req_count += 1
        rsp = self._request_session.get(req_url, headers = hdrs)
        if rsp.status_code == 304:  # Not Modified
            self._refresh_count += 1
            rsp = self._rsp_cache[req_url]
            self._rsp_fetched[req_url] = datetime.datetime.now()
        elif rsp.status_code == 200:
            self._rsp_cache[req_url] = rsp
            self._rsp_fetched[req_url] = datetime.datetime.now()
        elif rsp.status_code == 404 and notFoundOK:
            self._rsp_cache[req_url] = self.NotFound
            self._rsp_fetched[req_url] = datetime.datetime.now()
            return self.NotFound
        else:
            rsp.raise_for_status()
        return rsp

    def _get_file_contents_raw(self, target_filepath, branch, alt_repo_url=None):
        rsp = self._get_file_contents_info(target_filepath, branch, alt_repo_url=None)
        if rsp != self.NotFound:
            if rsp['encoding'] != 'base64':
                logging.error('Unknown encoding for %s, branch %s, repo %s: %s',
                              target_filepath, branch, alt_repo_url or self._url)
                return self.NotFound
            return base64.b64decode(rsp['content']).decode('utf-8')
        return rsp

    def get_gitmodules(self, reponame, branch, repo_src_url):
        srcurl = self.get_api_url(repo_src_url) if repo_src_url else repo_src_url
        rsp = self._get_file_contents_raw('.gitmodules', branch, repo_src_url)
        if rsp == self.NotFound:
            return GitmodulesRepoVers(reponame, branch, [])
        return self.parse_gitmodules_contents(reponame, branch, rsp, srcurl)

    def parse_gitmodules_contents(self, reponame, branch, gitmodules_contents, repo_src_url):
        gitmod_cfg = configparser.ConfigParser()
        gitmod_cfg.read_string(gitmodules_contents)
        ret = []
        for remote in gitmod_cfg.sections():
            # Note: if the URL of a repo moves, need a new name for the moved location?  Or choose not to track these changes?
            submod_info = self._get_file_contents_info(gitmod_cfg[remote]['path'], branch, repo_src_url)
            if submod_info == self.NotFound:
                # Is the repo in .gitmodules valid?
                valid_repo = self.api_req('', repo_src_url, notFoundOK=True)
                if valid_repo == self.NotFound:
                    logging.warning('Invalid URL for submodule %s, %s: using "%s"',
                                    remote, repo_src_url, os.path.split(gitmod_cfg[remote]['path'])[-1])
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
                    logging.warning('specified submodule revision %s does not exist in target repo %s as %s',
                                     branch, submod_info['name'], repo_src_url)
                    ret.append(SubRepoVers(submod_info['name'], repo_src_url, 'unknownRemoteRefForPullReq'))
            else:
                ret.append(self._subrepo_version(remote, gitmod_cfg[remote], submod_info))
        return GitmodulesRepoVers(reponame, branch, ret, alt_repo_url=repo_src_url)


class GitLabInfo(RemoteGit__Info):
    """Retrieve information from gitlab via API with cacheing.  Note that
       this object does not maintain a "name" for the repo because
       several projects may share the same repo.
    """
    def __init__(self, url, request_auth=None):
        super(GitLabInfo, self).__init__(self.get_api_url(url))
        if isinstance(request_auth, str):
            self._request_session.headers.update({'Private-Token': request_auth})

    def get_api_url(self, url):
        parsed = urlparse(self.to_http_url(url))
        return urlunparse(
            parsed._replace(path = 'api/v4/projects/' + parsed.path[1:].replace('/', '%2F')))

    @staticmethod
    def _fix_subrepo_source_url(url):
        parsed = urlparse(self._url)
        return urlunparse(parsed._replace(path='/api/v4/projects/%d'%pr['source_project_url']))

    def get_pullreqs(self, reponame):
        rsp = self.api_req('/merge_requests')
        # May want to filter on ["state"] == "open"
        # May want to echo either ["number"] or ["title"]
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        # ["head"]["repo"]["url"] is the github repo url for the source repo of the PR
        preqs = [ PullReqInfo(pr["id"],   # for user reference
                              pr["title"],    # for user reference
                              self._fix_subrepo_source_url(pr['source_project_url']),  # source repo URL
                              pr["source_branch"],          # source repo branch
                              pr["sha"])
                  for pr in rsp if pr["state"] == "opened" and not pr["merged_at"] ]
        return PullReqsData(reponame, preqs)

    def get_branches(self):
        return self.api_req('/repository/branches')

    def _get_file_contents_info(self, target_filepath, branch, alt_repo_url=None):
        return self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '?ref=' + branch)

    def _get_file_contents_raw(self, target_filepath, branch, alt_repo_url=None):
        return self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '/raw?ref=' + branch, raw=True)

    def _subrepo_version(self, remote_name, remote_info, submod_info):
        return SubRepoVers(submod_info['file_name'],
                           self.to_http_url(remote_info['url']),
                           submod_info['blob_id'])


class GitHubInfo(RemoteGit__Info):
    """Retrieve information from github via API with cacheing.  Note that
       this object does not maintain a "name" for the repo because
       several projects may share the same repo.
    """
    def __init__(self, url, request_auth=None):
        super(GitHubInfo, self).__init__(self.get_api_url(url))
        if isinstance(request_auth, requests.auth.AuthBase):
            self._request_session.auth = request_auth

    def get_api_url(self, url):
        """Converts a remote repository URL into a form that is useable for
           the Github API (https://developer.github.com/v3) to allow
           API-related requests.
        """
        parsed = urlparse(self.to_http_url(url))
        if parsed.netloc == 'github.com':
            return urlunparse(
                parsed._replace(netloc = 'api.github.com',
                                path = 'repos' + self._remove_trailer(parsed.path)))
        raise RuntimeError("No API URL parsing for: %s [ %s ]" % (url, str(parsed)))

    def get_pullreqs(self, reponame):
        rsp = self.api_req('/pulls')
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        preqs = [ PullReqInfo(pr["number"],   # for user reference
                              pr["title"],    # for user reference
                              pr["head"]["repo"]["html_url"],  # source repo URL
                              pr["head"]["ref"],          # source repo branch
                              pr["merge_commit_sha"])
                  for pr in rsp if pr["state"] == "open" and not pr["merged_at"] ]
        return PullReqsData(reponame, preqs)

    def get_branches(self):
        return self.api_req('/branches')

    def _get_file_contents_info(self, target_filepath, branch, alt_repo_url=None):
        return self.api_req('/contents/' + target_filepath + '?ref=' + branch,
                            repo_url_override=alt_repo_url, notFoundOK=True)

    def _subrepo_version(self, remote_name, remote_info, submod_info):
        if submod_info['type'] != 'submodule':
            logging.warning('Found %s at %s, but expected a submodule',
                            submod_info['type'], gitmod_cfg[remote]['path'])
            return None # ignore this submodule entry
        return SubRepoVers(submod_info['name'],
                           self.to_http_url(submod_info['submodule_git_url']),
                           submod_info['sha'])
