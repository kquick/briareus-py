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


LocalCachePeriod = datetime.timedelta(minutes=1, seconds=35)


@attr.s(frozen=True)
class RepoAPI_Location(object):
    apiloc = attr.ib()   # HTTP API URL base used internally to get
                         # information.  This may not yet be a valid
                         # API reference because there is often path
                         # or URL adjustments based on that, but this
                         # should be an http reference to an API
                         # server (e.g. a git forge) instead of an ssh
                         # or http reference to a repository.
    apitoken = attr.ib() # Token used to access the API URL (or None if no token)


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

    @staticmethod
    def repo_url(self, url):
        "Drops any additional path elements beyond the first two: owner and repo"
        parsed = urlparse(url)
        return urlunparse(
            parsed._replace(path = '/'.join(parsed.path.split('/')[:3]) ))

    def api_req(self, reqtype, notFoundOK=False, raw=False):
        self._get_count += 1
        if reqtype.startswith('//'):
            # Drop the owner/repo at the tail of the url
            parsed = urlparse(self._url)
            req_url = urlunparse(parsed._replace(path='/'.join(parsed.path.split('/')[:-2] + [reqtype[2:]])))
        else:
            req_url = self._url + reqtype
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

    def api_post(self, reqtype, data):
        # POST operations are not cached
        req_url = self._url + reqtype
        rsp = self._request_session.post(req_url, json=data)
        # if rsp.status_code == 201:
        #     return rsp
        rsp.raise_for_status()
        return rsp

    def get_file_contents_raw(self, target_filepath, branch):
        rsp = self._get_file_contents_info(target_filepath, branch)
        if rsp != self.NotFound:
            if rsp['encoding'] != 'base64':
                logging.error('Unknown encoding for %s, branch %s, repo %s: %s',
                              target_filepath, branch, self._url)
                return self.NotFound
            return base64.b64decode(rsp['content']).decode('utf-8')
        return rsp

    def get_gitmodules(self, reponame, branch, pullreq_id):
        rsp = self.get_file_contents_raw('.gitmodules', branch)
        if rsp == self.NotFound:
            return GitmodulesRepoVers(reponame, branch, pullreq_id, [])
        return self.parse_gitmodules_contents(reponame, branch, pullreq_id, rsp)

    def parse_gitmodules_contents(self, reponame, branch, pullreq_id, gitmodules_contents):
        gitmod_cfg = configparser.ConfigParser()
        gitmod_cfg.read_string(gitmodules_contents)
        ret = []
        for remote in gitmod_cfg.sections():
            # Note: if the URL of a repo moves, need a new name for the moved location?  Or choose not to track these changes?
            submod_info = self._get_file_contents_info(gitmod_cfg[remote]['path'], branch)
            if submod_info == self.NotFound:
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
                ret.append(self._subrepo_version(remote, gitmod_cfg[remote], submod_info))
        return GitmodulesRepoVers(reponame, branch, pullreq_id, ret)


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
    def __init__(self, repo_api_location):
        super(GitLabInfo, self).__init__(self.get_api_url(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.headers.update({'Private-Token': repo_api_location.apitoken})

    def get_api_url(self, url):
        parsed = urlparse(url)
        return urlunparse(
            parsed._replace(path = 'api/v4/projects/' + parsed.path[1:].replace('/', '%2F')))


    def _src_repo_url(self, mergereq):
        if 'source_project_url' in mergereq:
            return mergereq['source_project_url']
        # It's a source_project_id, but since it's on this gitlab
        # forge, it's in this repo as a local branch.  The proper URL
        # is not known here, only the forge API url, so defer the
        # actual URL to the caller who does have that information.
        if mergereq.get('source_project_id', "no_spid") == mergereq.get('target_project_id', "no_tpid"):
            return "SameProject"
        rsp = self.api_req('//projects/%d' % mergereq.get('source_project_id', "no_spid"), notFoundOK=True)
        if rsp == self.NotFound:
            return rsp
        return ("DifferentProject", rsp.name)

    def get_pullreqs(self, reponame):
        rsp = self.api_req('/merge_requests')
        # Gather {"upvotes": 0, "downvotes": 0, "approvals_before_merge": 0} for analysis phase
        # Use {"work_in_progress": true} to ignore the PR
        # Use {"merge_status": "can_be_merged"} for analysis phase?

        # n.b. GitLab pullreqs have an id and and iid.  The iid is the
        # one that is presented to the user on the Web page.

        preqs = []
        for pr in rsp:
            if pr["state"] == "opened" and not pr["merged_at"]:
                src_repo_url = self._src_repo_url(pr)
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
                    # check out the source repo to build against.  At
                    # this point, the only real option available at
                    # this time is to just ignore this merge request.
                    # Sorry!
                    logging.warning('Inaccessible source repo for gitlab repo %s PR #%d "%s"; ignoring',
                                    reponame, pr['iid'], pr['title'])
                else:
                    prinfo = PullReqInfo(str(pr["iid"]),   # for user reference
                                         pullreq_title=pr["title"],    # for user reference
                                         pullreq_srcurl=src_repo_url,
                                         pullreq_branch=pr["source_branch"],          # source repo branch
                                         pullreq_ref=pr["sha"],
                                         pullreq_user=pr['author']['username'],
                                         pullreq_email=self.get_user_email(pr['author']['id']),
                                         pullreq_mergeref=None)
                    preqs.append(prinfo)

        return PullReqsData(reponame, preqs)

    def get_user_email(self, userid):
        userinfo = self.api_req('//users/' + str(userid), notFoundOK=True)
        if userinfo == self.NotFound:
            return ''
        return userinfo['public_email']

    def get_branches(self):
        return self.api_req('/repository/branches')

    def _get_file_contents_info(self, target_filepath, branch):
        return self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '?ref=' + branch)

    def get_file_contents_raw(self, target_filepath, branch):
        return self.api_req('/repository/files/' + target_filepath.replace('/', '%2F') + '/raw?ref=' + branch,
                            # Sometimes this cannot be accessed, and
                            # the higher levels handle this.  This
                            # will frequently happen when someone
                            # forks a Private repo where a PAT has
                            # been added for the Private repo but the
                            # user's fork doesn't propagate the PAT.
                            notFoundOK=True,
                            raw=True)

    def _subrepo_version(self, remote_name, remote_info, submod_info):
        return SubRepoVers(submod_info['file_name'],
                           remote_info['url'],
                           submod_info['blob_id'])

    def set_commit_status(self, sts, desc, commitref, url='', context_ref=''):
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
        })


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
    def __init__(self, repo_api_location):
        super(GitHubInfo, self).__init__(self.get_api_url(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.auth = requests.auth.HTTPBasicAuth(
                *tuple(repo_api_location.apitoken.split(':')))

    def get_api_url(self, url):
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

    def get_pullreqs(self, reponame):
        rsp = self.api_req('/pulls')
        # May want to filter on ["state"] == "open"
        # May want to echo either ["number"] or ["title"]
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        # ["head"]["repo"]["url"] is the github repo url for the source repo of the PR
        # ["base"]["ref"] is the fork point the pull req is related to (e.g. matterhorn "develop")  # constrains merge command, but not build config...
        preqs = [ PullReqInfo(str(pr["number"]),   # for user reference
                              pullreq_title=pr["title"],    # for user reference
                              pullreq_srcurl=pr["head"]["repo"]["html_url"],  # source repo URL
                              pullreq_branch=pr["head"]["ref"],          # source repo branch
                              pullreq_ref=pr["head"]["sha"],         # for github, can also use branch ^
                              pullreq_user=pr["user"]["login"],
                              pullreq_email=self.get_user_email(pr["user"]["login"]),
                              pullreq_mergeref=pr["merge_commit_sha"])
                  for pr in rsp if pr["state"] == "open" and not pr["merged_at"] ]
        return PullReqsData(reponame, preqs)

    def get_user_email(self, username):
        userinfo = self.api_req('//user/' + username, notFoundOK=True)
        if userinfo == self.NotFound:
            return ''
        return userinfo['email'] or ''

    def get_branches(self):
        return self.api_req('/branches')

    def _get_file_contents_info(self, target_filepath, branch):
        return self.api_req('/contents/' + target_filepath + '?ref=' + branch, notFoundOK=True)

    def _subrepo_version(self, remote_name, remote_info, submod_info):
        if submod_info['type'] != 'submodule':
            logging.warning('Found %s at %s, but expected a submodule',
                            submod_info['type'], gitmod_cfg[remote]['path'])
            return None # ignore this submodule entry
        return SubRepoVers(submod_info['name'],
                           submod_info['submodule_git_url'],
                           submod_info['sha'])

    def set_commit_status(self, sts, desc, commitref, url='', context_ref=''):
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
        })


# ----------------------------------------------------------------------
# Support functions

def _remove_trailer(path, trailer):
    trailer_len = len(trailer)
    return path[:-trailer_len] if path[-trailer_len:] == trailer else path

def _changeloc(url, repolocs):
    parsed = urlparse(url)
    for each in repolocs:
        if parsed.netloc == each.repo_loc:
            return urlunparse(parsed._replace(netloc=each.api_host)), each.api_host, parsed.netloc
    return url, parsed.netloc, parsed.netloc

def to_http_url(url, repolocs):
    """Converts git clone access specification
    (e.g. "git@foo.com:group/proj") to the corresponding HTTP forge
    reference RepoAPI_URL (e.g. "https://foo.com/group/proj").  Also
    works if the source ends with ".git".

    Performs any network location translations specified in the xlate
    list (which has (from, to) pairs in it as commonly specified by
    the RepoLoc input specification.

    Returns the translated URL along with any access token for that
    URL (as extracted from the BRIAREUS_PAT environment variable).

    """
    if url.startswith("git@"):
        trimmed_url = _remove_trailer(url[len('git@'):], '.git')
        spl = trimmed_url.split(':')
        return to_http_url('https://%s/%s' % (spl[0], ':'.join(spl[1:])), repolocs)

    returl, for_remote, orig_remote_spec = _changeloc(_remove_trailer(url, '.git'), repolocs)

    patspec = os.getenv('BRIAREUS_PAT')
    if patspec is None:
        return RepoAPI_Location(returl, None)
    # The BRIAREUS_PAT format: "remote=PATSPEC;...", where PATSPEC
    # varies by the type of forge.  For Github, the PATSPEC is
    # "user:token".  For Gitlab, the PATSPEC is simply "token".
    patlist = patspec.split(';')

    # First try finding a PAT using the original target specification,
    # in case there is a specific PAT associated with a specific
    # remote.
    for pat in patlist:
        if pat.startswith(orig_remote_spec + '='):
            patval = pat[len(for_remote)+1:]
            return RepoAPI_Location(returl, patval)

    # Now try with the target remote to get a general translation
    for pat in patlist:
        if pat.startswith(for_remote + '='):
            patval = pat[len(for_remote)+1:]
            return RepoAPI_Location(returl, patval)

    return RepoAPI_Location(returl, None)

def to_access_url(url, for_repo, repolocs): # KWQ: use for_repo to get "git@" portion instead of https portion...
    """The Repo specification in the input may use a git ssh reference to
       a repo (e.g. "git@myproj-github:foo/bar") which indicates that
       an SSH deploy key is being used by the Builder (e.g. Hydra) to
       access the repository.

       This function should be called with any URL that the builder
       might use to access a version of that repository (e.g. a pull
       request or merge request reference, or a submodule), and it
       will translate the URL *back* into the form that the builder
       will need to use to access that repository.  If no translation
       is needed, the input URL is returned untouched.

    """
    if not for_repo:
        # URL is not for a primary input repo.  It is probably a
        # subrepo.  Because it is not a primary, there is no
        # translation information available.
        return url

    if not for_repo.repo_url.startswith('git@'):
        # Primary input repo doesn't use SSH access, so presumably
        # repo is publicly accessible and the URL will work
        return url

    # The for_repo specification indicates that SSH access is needed,
    # so extract the hostname so that the same hostname can be used in
    # the input URL to ensure the same Builder's .ssh/config entry is
    # used.

    trimmed_url = _remove_trailer(for_repo.repo_url[len('git@'):], '.git')
    ssh_host = trimmed_url.split(':')[0]

    # The input URL can be in http form or already in ssh form.
    if url.startswith("git@"):
        url_path = _remove_trailer(url[len('git@'):], '.git').split(':')[1]
    else:
        parsed = urlparse(url)
        # n.b. assumes there are no params, query, or fragment
        # portions of a git http URL.  Remove the initial slash on the
        # path part as well.
        url_path = parsed.path[1:]

    return "git@" + ssh_host + ":" + url_path
