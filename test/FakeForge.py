from urllib.parse import urlparse, urlunparse
import http.server
import threading
import requests
import pytest
import json


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
