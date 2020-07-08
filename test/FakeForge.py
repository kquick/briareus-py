from urllib.parse import urlparse, urlunparse
import http.server
import threading
import requests
import pytest
import json


local_github_port = 4234
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
    server = http.server.HTTPServer(('', local_github_port), FakeGitHub)
    server.response_data = request.module.forge_responses
    srun = threading.Thread(target=run_server(server))
    srun.start()
    yield srun
    r = requests.get('http://localhost:%d/shutdown'%local_github_port)
    srun.join(5)

def get_github_api_url_local(url):
    parsed = urlparse(url)
    return urlunparse(parsed._replace(netloc='localhost:%d'%local_github_port,
                                      scheme='http',
                                      path = 'repos' + parsed.path))
