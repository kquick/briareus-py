from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import base64
import json
from test_prsubmodule import expected_repo_info
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents,
                       github_branch,
                       gitlab_branch,
                       github_pullreq,
                       github_user)


input_spec = open('test/inp_prsubmodule').read()

fakeforge_port=4343


@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(input_spec, actor_system=actor_system)
    assert repo_info == expected_repo_info


forge_responses = {

    '/repos/toprepo_loc/branches':
    json.dumps([
        github_branch('TopRepo', 'https://github.com/toprepo_loc', 'master'),
    ]).encode('utf-8'),

    '/repos/subrepo_loc/branches':
    json.dumps([
        github_branch('SubRepo1', 'https://github.com/subrepo_loc', 'master'),
    ]).encode('utf-8'),

    '/repos/toprepo_loc/pulls?state=all': b'[]',

    '/repos/subrepo_loc/pulls?state=all':
    json.dumps([
        github_pullreq('https://github.com/subrepo_loc', '312',
                       'subfix', 'sr1pr312sf3', 'better',
                       'https://github.com/subrepo1_pr312_loc',
                       'dev', 1819),
    ]).encode('utf-8'),

    '/repos/toprepo_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/toprepo_loc', 'master',
                               b'''
[submodule "SubRepo1"]
  path = subrepo_path
  url = https://github.com/subrepo_loc
                               '''),

    '/repos/toprepo_loc/contents/subrepo_path?ref=master':
    github_submodule_contents('https://github.com/toprepo_loc',
                              'master',
                              'SubRepo1',
                              'subrepo_path',
                              'subrepo_master_head',
                              'https://github.com/subrepo_loc'),

    '/users/dev': github_user('https://github.com/toprepo_loc',
                              'dev', 243, 'dev@soft.ware'),
}
