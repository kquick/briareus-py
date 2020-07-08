from urllib.parse import urlparse, urlunparse
import http.server
import threading
import requests
import pytest
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
github_sha = lambda c: hashlib.sha1(c.encode('utf-8')).hexdigest()
github_nid = lambda v: base64.b64encode(v.encode('utf-8'))
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
    return json.dumps(
        {
            "name": ".gitmodules",
            "path": ".gitmodules",
            "sha": sha,
            "size": len(contents),
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
            "content": contents,
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
