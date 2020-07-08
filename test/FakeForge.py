from urllib.parse import urlparse, urlunparse
import http.server
import threading
import requests
import pytest
import base64
import json
import random
import hashlib


server_done = False

class FakeGitHub(http.server.BaseHTTPRequestHandler):
    def do_GET(self, *args):
        # print('GET for',args,'and path',self.path)
        if self.path == '/shutdown':
            global server_done
            server_done.set()
            self.send_response(200, 'Bye!')
            self.end_headers()
            self.wfile.write(b'shutting down as requested')
        elif self.path in self.server.response_data:
            # == '/repos/the_repo_url/branches':
            self.send_response(200)
            self.end_headers()
            self.wfile.write(self.server.response_data[self.path])
        else:
            self.send_response(404, 'Not Found')
            self.end_headers()
            self.wfile.write(json.dumps({'message': 'Not Found',
                                         'documentation_url': 'doc url',
            }).encode('utf-8'))

def run_server(server):
    def run():
        global server_done
        server_done.clear()
        while not server_done.is_set():
            server.handle_request()
    return run

@pytest.fixture(scope="module")
def fake_forge(request):
    global server_done
    server_done = threading.Event()
    server = http.server.HTTPServer(('', request.module.fakeforge_port), FakeGitHub)
    server.response_data = request.module.forge_responses
    srun = threading.Thread(target=run_server(server))
    srun.start()
    yield srun
    r = requests.get('http://localhost:%d/shutdown'%request.module.fakeforge_port)
    srun.join(5)

def get_github_api_url_local(fakeforge_port):
    def g(url):
        parsed = urlparse(url)
        return urlunparse(parsed._replace(netloc='localhost:%d'%fakeforge_port,
                                          scheme='http',
                                          path = 'repos' + parsed.path))
    return g

def get_gitlab_api_url_local(fakeforge_port):
    def g(url):
        parsed = urlparse(url)
        return urlunparse(parsed._replace(
            netloc='localhost:%d'%fakeforge_port,
            scheme='http',
            path = 'api/v4/projects/' + parsed.path[1:].replace('/', '%2F')))
    return g


gh_api_url = lambda url: url.replace('github.com/', 'api.github.com/repos/')
github_sha = lambda c: hashlib.sha1(c).hexdigest()
github_nid = lambda v: base64.b64encode(v.encode('utf-8')).decode('utf-8')
github_id  = random.randint(1,100000)

# def github_repo(url, name, owner='me'):
#     r_node_id = github_nid(url)
#     r_id = github_id()
#     o_node_id = github_id(owner)
#     o_id = github_id()
#     return json.dumps(
#         {
#             'id': r_id,
#             'node_id': r_node_id,
#             'name': name,
#             'full_name': 'project/' + name,
#             'private': False,
#             'owner' : {
#                 'login': owner,
#                 '
#         }
#     ).encode('utf-8')

def github_gitmodules_contents(primary_repo_url, branch, contents):
    sha = github_sha(contents)
    b64contents = base64.b64encode(contents).decode('utf-8')
    return json.dumps(
        {
            "name": ".gitmodules",
            "path": ".gitmodules",
            "sha": sha,
            "size": len(b64contents),
            "url": (gh_api_url(primary_repo_url) +
                    '/contents/.gitmodules?ref=' + branch),
            "html_url": primary_repo_url + "/blob/master/.gitmodules",
            "git_url": (gh_api_url(primary_repo_url) +
                        "/git/blobs/" + sha),
            "download_url": (primary_repo_url.replace('github.com',
                                                      'raw.githubusercontent.com') +
                             "/master/.gitmodules"),
            "type": "file",
            "encoding": "base64",
            "content": b64contents,
        }
    ).encode('utf-8')


def github_submodule_contents(primary_repo_url, branch,
                              reponame, path, ref, subrepo_url):
    return json.dumps(
        {
            'name': reponame,
            'path': path,
            'sha': ref,
            'size': 0,
            'url': (gh_api_url(primary_repo_url)
                    + '/contents/' + path + '?ref=' + branch),
            'html_url': subrepo_url + '/tree/' + ref,
            'git_url': (gh_api_url(subrepo_url) + '/git/trees/' + ref),
            'download_url': None,
            'type': 'submodule',
            'submodule_git_url': subrepo_url,
        }
    ).encode('utf-8')

def gitlab_branch(reponame, repourl, branch, refbase=None):
    ref = '-'.join([reponame, refbase or branch, "ref"])
    return {
        "name": branch,
        "commit": {
            "id": ref,
            "short_id": '-'.join([reponame, branch, "shortref"]),
            "created_at": "2020-01-29T10:58:29.000-08:00",
            "parent_ids": None,
            "title": "Latest commit",
            "message": "Latest commit",
            "author_name": "Doctor Who",
            "author_email": "drwho@tardis.out",
            "authored_date": "2020-01-29T10:58:29.000-08:00",
            "committer_name": "Doctor Who",
            "committer_email": "drwho@tardis.out",
            "committed_date": "2020-01-29T10:58:29.000-08:00",
            "web_url": repourl + "/-/commit/" + ref,
        },
        "merged": False,
        "protected": True,
        "developers_can_push": False,
        "developers_can_merge": False,
        "can_push": True,
        "default": True,
        "web_url": "https://gitlab.com/dalek/disabler/-/tree/" + branch,
    }

def github_branch(reponame, repourl, branch, refbase=None):
    ref = '-'.join([reponame, refbase or branch, "ref"])
    return {
        "name": branch,
        "commit": {
            "sha": ref,
            "url": repourl + "/commits/" + ref,
        },
        "protected": False,
    }

def github_pullreq(repourl, pr_id, branch, ref,
                   title, pr_src_url,
                   user, userid,
                   repoowner=None, repoowner_id=None,
                   state=None):
    reponame = repourl.split('/')[-1]
    pr_node_id = github_nid(title)  # title is randomly chosen here
    repo_id = github_id
    rowner = repoowner or 'alfred'
    rowner_id = repoowner_id or random.randint(1, 10302)
    return {
        "url": repourl + '/pulls/' + pr_id,
        "id": random.randint(100000000, 999999999),
        "node_id": pr_node_id,
        "html_url": repourl + '/pull/' + pr_id,
        "diff_url": repourl + "/pull/" + pr_id + ".diff",
        "patch_url": repourl + "/pull/" + pr_id + ".patch",
        "issue_url": repourl + "/issues/" + pr_id,
        "number": int(pr_id),
        "state": state or "open",
        "locked": False,
        "title": title,
        "user": {
            "login": user,
            "id": userid,
            "type": "User",
            "site_admin": False
        },
        "body": "",
        "created_at": "2020-01-21T17:35:14Z",
        "updated_at": "2020-01-26T23:35:44Z",
        "closed_at": None,
        "merged_at": None,
        "merge_commit_sha": ref + "_merge_commit",
        "assignee": None,
        "assignees": [],
        "requested_reviewers": [],
        "requested_teams": [],
        "labels": [],
        "milestone": None,
        "draft": False,
        "commits_url": repourl + "/pulls/" + pr_id + "/commits",
        "review_comments_url": repourl + "/pulls/" + pr_id + "/comments",
        "review_comment_url": repourl + "/pulls/comments{/number}",
        "comments_url": repourl + "/issues/" + pr_id + "/comments",
        "statuses_url": repourl + "/statuses/" + ref,
        "head": {
            "label": reponame + ':' + branch,
            "ref": branch,
            "sha": ref,
            "user": {
                "login": user,
                "id": userid,
                "type": "User",
                "site_admin": False
            },
            "repo": {
                "id": repo_id,
                "name": reponame,
                "full_name": "proj_repo/" + reponame,
                "private": False,
                "owner": {
                    "login": rowner,
                    "id": rowner_id,
                    "type": "User",
                    "site_admin": False,
                },
                "html_url": pr_src_url,
                "description": reponame + " repo",
                "fork": False,
                "created_at": "2020-01-21T17:32:49Z",
                "updated_at": "2020-01-21T17:37:42Z",
                "pushed_at": "2020-01-26T23:35:45Z",
                "homepage": None,
                "size": 3,
                "stargazers_count": 0,
                "watchers_count": 0,
                "language": None,
                "has_issues": False,
                "has_projects": True,
                "has_downloads": True,
                "has_wiki": True,
                "has_pages": False,
                "forks_count": 0,
                "mirror_url": None,
                "archived": False,
                "disabled": False,
                "open_issues_count": 1,
                "license": None,
                "forks": 0,
                "open_issues": 1,
                "watchers": 0,
                "default_branch": "master"
            }
        },
        "base": {
            "label": reponame + ":master",
            "ref": "master",
            "sha": reponame + "_master_ref",
            "user": {
                "login": rowner,
                "id": rowner_id,
                "type": "User",
                "site_admin": False,
            },
            "repo": {
                "id": repo_id,
                "name": reponame,
                "full_name": "proj_name/" + reponame,
                "private": False,
                "owner": {
                    "login": rowner,
                    "id": rowner_id,
                    "type": "User",
                    "site_admin": False,
                },
                "html_url": repourl,
                "description": reponame + " repo",
                "fork": False,
                "url": repourl,
                "created_at": "2020-01-21T17:32:49Z",
                "updated_at": "2020-01-21T17:37:42Z",
                "pushed_at": "2020-01-26T23:35:45Z",
                "homepage": None,
                "size": 3,
                "stargazers_count": 0,
                "watchers_count": 0,
                "language": None,
                "has_issues": False,
                "has_projects": True,
                "has_downloads": True,
                "has_wiki": True,
                "has_pages": False,
                "forks_count": 0,
                "mirror_url": None,
                "archived": False,
                "disabled": False,
                "open_issues_count": 1,
                "license": None,
                "forks": 0,
                "open_issues": 1,
                "watchers": 0,
                "default_branch": "master"
            }
        },
        "_links": {
            "self": {
                "href": repourl + "/pulls/" + pr_id,
            },
            "html": {
                "href": repourl + "/pull/" + pr_id,
            },
            "issue": {
                "href": repourl + "/issues/" + pr_id,
            },
            "comments": {
                "href": repourl + "/issues/" + pr_id + "/comments"
            },
            "review_comments": {
                "href": repourl + "/pulls/" + pr_id + "/comments"
            },
            "review_comment": {
                "href": repourl + "/pulls/comments{/number}"
            },
            "commits": {
                "href": repourl + "/pulls/" + pr_id + "/commits"
            },
            "statuses": {
                "href": repourl + "/statuses/" + ref
            }
        },
        "author_association": "OWNER",
        "active_lock_reason": None
    }


def github_user(repourl, username, userid, email=None):
    return json.dumps({
        'login': username,
        'id': userid,
        'node_id': github_nid(username),
        'url': repourl + '/users/' + username,
        'html_url': repourl + '/' + username,
        'type': 'User',
        'site_admin': False,
        'name': username.upper(),
        'company': email.split('@')[-1] if email else email,
        'blog': '',
        'location': 'Some ' + username + ', Location',
        'email': email,
        'hireable': None,
        'bio': None,
        'twitter_username': None,
        'public_repos': 34,
        'public_gists': 12,
        'followers': 56,
        'following': 78,
        "created_at": "2011-05-14T06:20:56Z",
        "updated_at": "2020-05-21T14:26:57Z",
    }).encode('utf-8')
