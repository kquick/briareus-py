from Briareus.VCS.InternalMessages import PullReqInfo, PRSts_Active
from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import base64
import json
import copy
from datetime import timedelta
import test_example1_gitdiscrete
from test_example4 import expected_repo_info
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       get_gitlab_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents,
                       gitlab_branch,
                       github_branch)


fakeforge_port = 4326

r10_key = lambda e: '/api/v4/projects/r10_url/repository' + e
r4_key = lambda e: '/repos/r4_explicit_default_url' + e

forge_responses = copy.deepcopy(test_example1_gitdiscrete.forge_responses)

forge_responses[r10_key('/branches')] = json.dumps([
    gitlab_branch("R10", "https://gitlab.com/r10_url", "develop",
                  refbase='master'),
]).encode('utf-8')
forge_responses[r10_key('/files/.gitmodules/raw?ref=master')] = \
    forge_responses[r10_key('/files/.gitmodules/raw?ref=master')] \
    .decode('utf-8') \
    .replace('/r4_url', '/r4_explicit_default_url') \
    .encode('utf-8')
forge_responses[r10_key('/files/.gitmodules/raw?ref=develop')] = \
    forge_responses[r10_key('/files/.gitmodules/raw?ref=master')]
forge_responses[r10_key('/files/sub%2Fr3?ref=develop')] = \
    forge_responses[r10_key('/files/sub%2Fr3?ref=master')]
forge_responses[r10_key('/files/deps%2Fr4?ref=develop')] = \
    forge_responses[r10_key('/files/deps%2Fr4?ref=master')]
forge_responses[r4_key('/branches')] = json.dumps([
    github_branch("R4", r4_key(''), "primary"),  # refbase='master'),
    github_branch("r4", r4_key(''), "feat1"),
]).encode('utf-8')
forge_responses[r4_key('/pulls?state=all')] = \
    forge_responses['/repos/r4_url/pulls?state=all']



input_spec = open('test/inp_example4').read()


@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
@patch.object(Briareus.VCS.GitForge.GitLabInfo, 'get_api_url')
def test_gitinfo(get_lab_api_url, get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)
    get_lab_api_url.side_effect = get_gitlab_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(input_spec, actor_system=actor_system)
    assert expected_repo_info == repo_info


# #######################################################################


forge_stats = {

    'requests': {
        n:1 for n in [
            '/api/v4/projects/r10_url/merge_requests?scope=all&state=all',
            '/api/v4/projects/r10_url/repository/branches',
            '/api/v4/projects/r10_url/repository/files/.gitmodules/raw?ref=develop',

            '/api/v4/projects/r10_url/repository/files/deps%2Fr4?ref=develop',
            '/api/v4/projects/r10_url/repository/files/sub%2Fr3?ref=develop',
            '/repos/r3_url/branches',
            '/repos/r3_url/pulls?state=all',
            '/repos/r4_explicit_default_url/branches',
            '/repos/r4_explicit_default_url/pulls?state=all',
            '/users/done',
            '/users/nick',
            '/users/ozzie',
        ]},

    'responses': {
        n:[404] for n in [
        ]},

}
