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
        self._ghinfo = GitHubInfo(self.repospec.repourl, request_auth=msg.request_auth)

    def receiveMsg_GetPullReqs(self, msg, sender):
        try:
            rsp = self._ghinfo.get_pullreqs(msg.reponame)
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  str(err)))
        else:
            self.send(msg.orig_sender, rsp)

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        try:
            rsp = self._ghinfo.get_branches()
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  str(err)))
        else:
            blist = [ b['name'] for b in rsp ]
            chk = branch in blist
            self.send(msg.orig_sender, BranchPresent(msg.reponame, branch, chk))

    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        try:
            rval = self._ghinfo.get_gitmodules(msg.reponame, branch,
                                               repo_src_url=msg.alt_repo_url)
        except Exception as err:
            self.send(msg.orig_sender,
                      InvalidRepo(msg.reponame, 'git', self.repospec.repourl, self.repo_cachedir,
                                  str(err)))
        else:
            self.send(msg.orig_sender, rval)


    def receiveMsg_str(self, msg, sender):
        if msg == "status":
            self.send(sender, self._ghinfo.stats() if self._ghinfo else
                      { "url": str(self.repospec.repourl) + " (never accessed)",
                      })


class GitHubInfo(object):
    """Retrieve information from github via API with cacheing.  Note that
       this object does not maintain a "name" for the repo because
       several projects may share the same repo.
    """

    NotFound = 404

    def __init__(self, url, request_auth=None):
        self._url = self.get_api_url(url)
        self._request_session = requests.Session()
        self._request_session.auth = request_auth
        self._rsp_cache = {}
        self._rsp_fetched = {}
        self._get_count = 0
        self._req_count = 0
        self._refresh_count = 0

    def stats(self):
        return { "url": self._url,
                 "rsp_cache_keys": list(self._rsp_cache.keys()),
                 "get_info_reqs": self._get_count,
                 "github_reqs": self._req_count,
                 "github_refreshes": self._refresh_count
                 # n.b. get_info_reqs - github_reqs - len(rsp_cache_keys) = error or 404 responses
        }

    trailer = '.git'
    trailer_len = len(trailer)

    def _remove_trailer(self, path):
        return (path[:-self.trailer_len]
                if path[-self.trailer_len:] == self.trailer
                else path)

    def get_api_url(self, url):
        """Converts a remote repository URL into a form that is useable for
           the Github API (https://developer.github.com/v3) to allow
           API-related requests.
        """
        if url.startswith("git@"):
            trimmed_url = self._remove_trailer(url[len('git@'):])
            spl = trimmed_url.split(':')
            return self.get_api_url('https://%s/%s' % (spl[0], ':'.join(spl[1:])))
        parsed = urlparse(url)
        if parsed.netloc == 'github.com':
            return urlunparse(
                parsed._replace(netloc = 'api.github.com',
                                path = 'repos' + self._remove_trailer(parsed.path)))
        raise RuntimeError("No API URL parsing for: %s [ %s ]" % (url, str(parsed)))

    def github_req(self, reqtype, repo_url_override=None, notFoundOK=False):
        self._get_count += 1
        req_url = (repo_url_override or self._url) + reqtype
        last_one = self._rsp_cache.get(req_url, None)
        if last_one:
            # If fetched within the local cache period, just re-use
            # the same response
            last = self._rsp_fetched.get(req_url, None)
            if last:
                if datetime.datetime.now() - last < LocalCachePeriod:
                    ret = self._rsp_cache[req_url]
                    if ret == self.NotFound:
                        return ret
                    return ret.json()
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
        return rsp.json()

    def get_pullreqs(self, reponame):
        rsp = self.github_req('/pulls')
        # May want to filter on ["state"] == "open"
        # May want to echo either ["number"] or ["title"]
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        # ["head"]["repo"]["url"] is the github repo url for the source repo of the PR
        preqs = [ PullReqInfo(pr["number"],   # for user reference
                              pr["title"],    # for user reference
                              pr["head"]["repo"]["html_url"],  # source repo URL
                              pr["head"]["ref"],          # source repo branch
                              pr["merge_commit_sha"])
                  for pr in rsp if pr["state"] == "open" and not pr["merged_at"] ]
        return PullReqsData(reponame, preqs)

    def get_branches(self):
        return self.github_req('/branches')

    def get_gitmodules(self, reponame, branch, repo_src_url):
        srcurl = self.get_api_url(repo_src_url) if repo_src_url else repo_src_url
        rsp = self.github_req('/contents/.gitmodules?ref=' + branch, srcurl, notFoundOK=True)
        if rsp == self.NotFound:
            return GitmodulesRepoVers(reponame, branch, [])
        return self.parse_gitmodules_response(reponame, branch, rsp, srcurl)

    def parse_gitmodules_response(self, reponame, branch, rsp, repo_src_url):
        if rsp['encoding'] != 'base64':
            raise RuntimeError('Unrecognized encoding for .gitmodules: ' + rsp['encoding'])
        gitmodules_contents = base64.b64decode(rsp['content']).decode('utf-8')
        gitmod_cfg = configparser.ConfigParser()
        gitmod_cfg.read_string(gitmodules_contents)
        ret = []
        for remote in gitmod_cfg.sections():
            # Note: if the URL of a repo moves, need a new name for the moved location?  Or choose not to track these changes?
            submod_info = self.github_req('/contents/' + gitmod_cfg[remote]['path'] + '?ref=' + branch,
                                          repo_src_url,
                                          notFoundOK=True)
            # TODO: instead of submitting these directly here, defer
            # them to the GitRepoInfo associated with the target URL?
            # This would help amortize if the same source target was
            # used in multiple repos (unlikely).
            if submod_info == self.NotFound:
                # The submodule was added to .gitmodules, but no
                # actual version of the remote repo was committed, so
                # no reference SHA can be known.  Instead, use a
                # reference sha that is completely invalid, which
                # should cause a build failure.
                submod_info = self.github_req('', repo_src_url, notFoundOK=True)
                if submod_info == self.NotFound:
                    # The submodule added to .gitmodules specified an
                    # invalid repository.  Have to assume the name is
                    # the last component of the path.
                    ret.append(SubRepoVers(os.path.split(gitmod_cfg[remote]['path'])[-1],
                                           'invalid_remote_repo',
                                           'unknownRemoteRefForPullReq'))
                else:
                    ret.append(SubRepoVers(submod_info['name'], repo_src_url, 'unknownRemoteRefForPullReq'))
            else:
                if submod_info['type'] != 'submodule':
                    logging.warning('Found %s at %s, but expected a submodule', submod_info['type'], gitmod_cfg[remote]['path'])
                    pass # ignore this submodule entry
                else:
                    ret.append(SubRepoVers(submod_info['name'],
                                           self._remove_trailer(submod_info['submodule_git_url']),
                                           submod_info['sha']))
        return GitmodulesRepoVers(reponame, branch, ret, alt_repo_url=repo_src_url)
