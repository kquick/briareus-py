"""This module provides functionality to access a git forge
(e.g. github or gitlab) using the GraphQL API interface.

"""

import attr
import requests
from urllib.parse import urlparse, urlunparse
from .ForgeAccess import *
from Briareus.VCS.InternalMessages import *


class NoDefault(object): pass

def g_(srcdict, *keys, defval=NoDefault()):
    here = srcdict
    for key in keys:
        if not isinstance(here, dict):
            if isinstance(defval, NoDefault):
                raise RuntimeError('Early leaf, missing ' + key + ' in ' + str(keys))
            return defval
        if key not in here:
            if isinstance(defval, NoDefault):
                raise RuntimeError('Missing entry for ' + key + ' in ' + str(keys))
            return defval
        here = here[key]
    return here


def _remove_trailer(trailer):
    trailer_len = len(trailer)
    return lambda path: path[:-trailer_len] if path[-trailer_len:] == trailer else path


class GitForge__BASE(object):
    "Common functionality for remote Git forge access (Github or Gitlab)."
    def __init__(self, name, api_url, owner, repo):
        self._name = name
        self._url = api_url
        self._owner = owner
        self._repo = repo
        self._request_session = requests.Session()
        self._rsp_cache = {} # key = request, value = response
        self._req_count = 0

    NotFound = 404

    def stats(self):
        return { "url": self._url,
                 "rsp_cache_keys": list(self._rsp_cache.keys()),
                 "remote_reqs": self._req_count,
        }


# ----------------------------------------------------------------------
#
# GitHub access
#
# Examples:
# $ CURL="curl -v https://api.github.com/graphql -X POST -H 'Authorization: bearer TOK'"
# $
# $ ${CURL} -d '{ "query": "query { viewer { name repositories(last: 3) { nodes { name }}}}" }'
# $
# $ ${CURL} -d '{ "query": "query { repository(owner:\"matterhorn-chat\", name:\"matterhorn\") {  name pullRequests(last:20) { nodes { title } }}}" }'
# $
# $ ${CURL}  -d '{ "query": "query { repository(owner:\"matterhorn-chat\", name:\"matterhorn\") {  name pullRequests(last:20, states:[OPEN]) { nodes { title commits(last:1) { nodes { url resourcePath id commit { author { name email user { company } } id message status { contexts { context description state } }  } } } } }}}" }'

class GitHub(GitForge__BASE):
    """Access GitHub via the API for GraphQL queries , with caching.  Note
       that this object does not maintain a "name" for the repo
       because several projects may share the same repo.

    """
    def __init__(self, name, repo_api_location):
        super(GitHub, self).__init__(name, *self.get_api_url_and_ident(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.headers['Authorization'] = 'bearer %s' % (
                repo_api_location.apitoken.split(':')[1])

    @staticmethod
    def get_api_url_and_ident(url):
        """Converts a remote repository URL:
               https://github.com/owner/repo
           into a form that is useable for
           the Github API (https://developer.github.com/v4) to allow
           API-related requests.
        """
        parsed = urlparse(url)
        if parsed.netloc == 'github.com':
            return (urlunparse(
                parsed._replace(netloc = 'api.github.com',
                                path = 'graphql')),
            ) + tuple(map(_remove_trailer('.git'), parsed.path.split('/')[1:3]))
        raise RuntimeError("No API URL parsing for: %s [ %s ]" % (url, str(parsed)))

    def get_VCS_info(self, get_submodules=list()):
        rawdata = self.get_VCS_rawdata(get_submodules)
        return {
            'branches': [
                BranchRef(reponame=self._name, branchname=br['name'], branchref=br['ref'])
                for br in rawdata['branches']],
            'pullreqs': rawdata['pullreqs'],
            'submodules': rawdata['submodules'],
            'subrepos': rawdata['subrepos'],
        }

    def get_VCS_rawdata(self, get_submodules=False):
        submodulequery = (
            # no pagination for submodules here,
            # assumes < 100 submodules for a repo
            ' submodules(first:100) { nodes {'
            '   name branch gitUrl subprojectCommitOid'
            ' } }'
            if get_submodules else '')
        branches = lambda num,at: ('refs(first:%(num)d, refPrefix:"refs/heads/" %(after)s)'
                                   ' { edges { cursor node { name target {'
                                   '    ... on Commit { oid '
                                   '     %(submodulequery)s'
                                   '}}}} '
                                   # '   totalCount'
                                   '   pageInfo { hasNextPage }'  # endCursor
                                   ' }'
                                   % { 'num': num,
                                       'after': 'after:"%s"' % at if at else '',
                                       'submodulequery': submodulequery,
                                   })
        pullreqs = lambda num,at: ('pullRequests(first:%(num)d, %(after)s) {'  #  states:[OPEN]
                                   '  edges {'
                                   '    cursor'
                                   '    node {'
                                   '      number state title headRepository { url } headRefName headRefOid'
                                   '      author { login }'
                                   # '      commits(first:1) { nodes { commit { author { email user { login } } } } }' # oid
                                   '      commits(last:1) { nodes { commit {'
                                   '        author { email user { login } }'
                                   '        %(submodulequery)s'
                                   '      } } }' # oid
                                   '    }'
                                   '  }'
                                   '  pageInfo { hasNextPage }'
                                   '}'
                                   % { 'num': num,
                                       'after': 'after:"%s"' % at if at else '',
                                       'submodulequery': submodulequery,
                                   })
        submodules = lambda num,at: ('submodules(first:%(num)d, %(after)s) {'
                                     '  edges {'
                                     '    cursor'
                                     '    node {'
                                     '      name branch gitUrl subprojectCommitOid'
                                     '    }'
                                     '  }'
                                     '  pageInfo { hasNextPage }'
                                     '}'
                                     % { 'num': num,
                                         'after': 'after:"%s"' % at if at else '',
                                     })
        ret = { 'branches': [],
                'pullreqs': [],
                'submodules': [],
                'subrepos': [],
        }
        next = { category: { 'num': 100, 'at': '' }
                 for category in
                 filter(None, [ 'branches', 'pullreqs',
                                'submodules' if get_submodules else None ])}
        complete = { 'branches': False,
                     'pullreqs': False,
                     'submodules': not get_submodules
        }
        while not all(complete.values()):
            query = ' '.join(['query { repository(owner:"%(owner)s", name:"%(repo)s") {',
                              branches(**next['branches']) if 'branches' in next else '',
                              pullreqs(**next['pullreqs']) if 'pullreqs' in next else '',
                              submodules(**next['submodules']) if 'submodules' in next else '',
                              '}',
                              # ' rateLimit { cost limit nodeCount }',
                              '}',
            ]) % { 'owner': self._owner, 'repo': self._repo }
            data = { 'query': query }
            print('\n\n>>> REQ\n',data,'\n\n')
            rsp = self._request_session.post(self._url, json=data)
            # if notFoundOK and rsp.status_code == 404:
            #     return self.NotFound
            # for err in rsp.json().get('errors', []):
            #     logging.error('POST %s err: %s', req_url,
            #                  err.get('message', rsp.status_code))
            if rsp.status_code != 200:
                logging.error('getVCSinfo for repo %s/%s: %s',
                              self._owner, self._repo, rsp)
            rsp.raise_for_status()
            rj = rsp.json()
            # print('<<< RESP\n',pformat(rj),'\n\n')
            # print('<<<COST',self._name,'>>>\n',pformat(g_(rj,'data','rateLimit',defval='<no rate info>')))
            rjr = rj['data']['repository']

            if 'submodules' in rjr:
                # note: these submodules do use Connections edges
                self._handle_subrepos(ret, None, None, g_(rjr, 'submodules', 'edges'), 'node')

            if 'refs' in rjr:
                ret['branches'].extend(
                    [ {'name': r['node']['name'],
                       'ref': r['node']['target']['oid']}
                      for r in rjr['refs']['edges'] ])
                for each in rjr['refs']['edges']:
                    # does not use Connections edges for submodules; assumes no
                    # more than 100 submodules, so no pagination support here
                    submods = g_(each, 'node', 'target', 'submodules', 'nodes', defval=list())
                    self._handle_subrepos(ret, g_(each, 'node', 'name'), None, submods)

            if 'pullRequests' in rjr:

                for each in rjr['pullRequests']['edges']:
                    # does not use Connections edges for submodules; assumes no
                    # more than 100 submodules, so no pagination support here
                    submods = g_(g_(each, 'node', 'commits', 'nodes')[0],
                                 'commit', 'submodules', 'nodes', defval=list())
                    self._handle_subrepos(ret, g_(each, 'node', 'headRefName'), str(g_(each, 'node', 'number')), submods)

                ret['pullreqs'].extend(
                    [ PullReqInfo(str(g_(p, 'node', 'number')),
                                  pullreq_status={'CLOSED' : PRSts_Closed(),
                                                  'MERGED' : PRSts_Merged(),
                                                  'OPEN'   : PRSts_Active() }[g_(p, 'node', 'state')],
                                  pullreq_title=g_(p, 'node', 'title'),
                                  pullreq_srcurl=_remove_trailer('.git')(g_(p, 'node', 'headRepository', 'url', defval='')),
                                  pullreq_branch=g_(p, 'node', 'headRefName'),
                                  pullreq_ref=g_(p, 'node', 'headRefOid'),
                                  # n.b. the "author" of a pull request only has the login, not the
                                  # email. A commit has an email and a login user, where the former
                                  # is supplied by the git commit and not the forge (although the
                                  # latter is probably the forge account used to authenticat?).  The
                                  # following gives preference to the commit in order to obtain the
                                  # email under the assumption that the author of the first commit
                                  # is the one that opens the pull request (not always true, and
                                  # sometimes true but using a different identity), but falls back
                                  # to the user with no email for git commits not supplying this
                                  # information (rare, but also possible, set matterhorn PR 581).
                                  # There is a "user" object that could be queried using the author
                                  # login, but most users do *not* provide a publicly visible email
                                  # address due to spam.
                                  pullreq_user=g_(g_(p, 'node', 'commits', 'nodes')[0], 'commit', 'author', 'user', 'login',
                                                  defval=g_(p, 'node', 'author', 'login', defval="who??")),
                                  pullreq_email=g_(g_(p, 'node', 'commits', 'nodes')[0], 'commit', 'author', 'email',
                                                   defval=''),
                                  pullreq_mergeref='')
                      for p in rjr['pullRequests']['edges'] ])


            for category,idx in [ ('branches', 'refs'),
                                  ('pullreqs', 'pullRequests'),
                                  ('submodules', 'submodules'),
            ]:
                complete[category] = (complete[category] or
                                      idx not in rjr or
                                      not rjr[idx]['pageInfo']['hasNextPage'])
                if complete[category]:
                    if category in next:
                        del next[category]
                else:
                    next[category]['at'] = rjr[idx]['edges'][-1]['cursor']

        return ret

    def _handle_subrepos(self, ret, branch_name, pullreq_id, submods, *extra_path):
        # Note: uses the last component of the submodule name as the
        # repo name.  This is under the user's control and might not
        # match the actual repo name.  This might result in two
        # references to the same repo with different names if the
        # input_desc already specified this repo with a different
        # name, but that should be benign.
        subrepo_name=lambda sr: g_(sr, *extra_path, 'name').split('/')[-1]
        t_ = _remove_trailer('.git')
        subs = [
            # See subrepo_name note above.
            SubRepoVers(subrepo_name=subrepo_name(sub),
                        subrepo_url=t_(g_(sub, *extra_path, 'gitUrl')),
                        subrepo_vers=g_(sub, *extra_path, 'subprojectCommitOid'))
            for sub in submods]

        ret['submodules'].append(
            GitmodulesRepoVers(
                reponame=self._name,
                branch_name=branch_name,
                pullreq_id=pullreq_id,
                gitmodules_repovers=subs))
        ret['subrepos'].extend([
            RepoSite(repo_name=subrepo_name(sub),
                     repo_url=t_(g_(sub, *extra_path, 'gitUrl')),
                     main_branch='master') # assumption
            for sub in submods
            if not any([sr.repo_url == t_(g_(sub, *extra_path, 'gitUrl')) for sr in ret['subrepos']])
        ])

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
        # This is still a v3 operation, unsupported by the v4 GraphQL API.

        return self._api_post('/statuses/' + commitref,
                              {"state": sts,
                               "description": desc,
                               "target_url": url,
                               "context": ("ci/briareus:" + context_ref
                                           if context_ref else "ci/briareus"),
                              },
                              # Allow 404: means no permissions to set a status, so don't retry
                              notFoundOK=True)

    def _api_post(self, reqtype, data, notFoundOK=False):
        # Generate a v3 URL for this reqtype
        req_url = self._url.replace('graphql',
                                    'repos/%s/%s%s'
                                    % (self._owner, self._repo, reqtype))
        print('posting to',req_url)
        print('  data',str(data))
        rsp = self._request_session.post(req_url, json=data)
        if notFoundOK and rsp.status_code == 404:
            print('not found but ok')
            print(rsp)
            print(rsp.text)
            return self.NotFound
        for err in rsp.json().get('errors', []):
            logging.error('POST %s err: %s', req_url,
                         err.get('message', rsp.status_code))
        rsp.raise_for_status()
        return rsp

    # Rate Limiting Notes
    # -------------------
    #
    # Github's limit is 5000 points/hour.
    #
    #   10 projects
    #     40 repos
    #       100 branches
    #       100 PRs
    #
    #   (10 * 40 * 2) / 100 = 402 / 100 = 4 points
    #
    #   4 points * 1 query/10 min * 60 min/hr = 24 points/hour (out of the 5000 limit)


# ----------------------------------------------------------------------
#
# GitLab access
#
# Examples:
#


class GitLab(GitForge__BASE):
    """Access GitLab via the API for GraphQL queries, with caching.  Note
       that this object does not maintain a "name" for the repo
       because several projects may share the same repo.

    """
    def __init__(self, name, repo_api_location):
        super(GitLab, self).__init__(name, self.get_api_url(repo_api_location.apiloc))
        if repo_api_location.apitoken:
            self._request_session.headers.update({'Private-Token': repo_api_location.apitoken})

    def get_api_url(self, url):
        parsed = urlparse(url)
        return urlunparse(
            parsed._replace(path = 'api/v4/projects/' + parsed.path[1:].replace('/', '%2F')))
