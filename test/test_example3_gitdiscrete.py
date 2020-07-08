from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
from test_example3 import expected_repo_info
import pytest
from unittest.mock import patch
from test_example1_gitdiscrete import forge_responses
from FakeForge import (fake_forge, get_github_api_url_local,
                       get_gitlab_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents)


input_spec = open('test/inp_example3').read()

# n.b. same responses as example1_gitdiscrete, but the server scope is
# module, so it may not exist after the original module is used.
fakeforge_port = 4323

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
    assert repo_info == expected_repo_info


forge_stats = {

    'requests': {
        n:1 for n in
        [
            '/api/v4/projects/r10_url/merge_requests?scope=all&state=all',
            '/api/v4/projects/r10_url/repository/branches',
            '/api/v4/projects/r10_url/repository/files/.gitmodules/raw?ref=master',
            '/api/v4/projects/r10_url/repository/files/deps%2Fr4?ref=master',
            '/api/v4/projects/r10_url/repository/files/sub%2Fr3?ref=master',
            '/repos/r3_url/branches',
            '/repos/r3_url/pulls?state=all',
            '/repos/r4_url/branches',
            '/repos/r4_url/pulls?state=all',
            '/users/done',
            '/users/nick',
            '/users/ozzie',
           ]
        },

    'responses': {},
}
