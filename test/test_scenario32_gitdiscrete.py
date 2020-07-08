from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import json
from test_scenario32 import (proj1_input_spec, expected_repo_proj1_info,
                             proj2_input_spec, expected_repo_proj2_info)
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents,
                       github_branch,
                       github_pullreq,
                       github_user)


fakeforge_port = 4350


@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(proj1_input_spec,
                                                    actor_system=actor_system)
    assert repo_info == expected_repo_proj1_info

@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(proj2_input_spec,
                                                    actor_system=actor_system)
    assert repo_info == expected_repo_proj2_info


repo1_master_gitmodules = b'''
[submodule "RepoA"]
  path = sub/repoA
  url = https://github.com/repoA_loc
'''

repo1_foo_gitmodules = b'''
[submodule "RepoA"]
  path = sub/repoA
  url = https://github.com/repoA_loc
'''

repo2_master_gitmodules = b'''
[submodule "RepoA"]
  path = deps/repoA
  url = https://github.com/repoA_loc
'''


forge_responses = {

    '/repos/repo1_loc/branches':
    json.dumps([
        github_branch('repo1', 'https://github.com/repo1_loc', 'master'),
        github_branch('repo1', 'https://github.com/repo1_loc', 'foo'),
    ]).encode('utf-8'),

    '/repos/repo2_loc/branches':
    json.dumps([
        { "name": "master",
          "commit": {
              "sha": "r2_master_ref",
              "url": "https://github.com/repo2_loc/commits/master",
          },
        },
        { "name": "dog",
          "commit": {
              "sha": "r2_dog_ref",
              "url": "https://github.com/repo2_loc/commits/dog",
          },
        },
    ]).encode('utf-8'),

    '/repos/repoA_loc/branches':
    json.dumps([
        { "name": "master",
          "commit": {
              "sha": "rA_master_ref",
              "url": "https://github.com/repoA_loc/commits/master",
          },
        },
        github_branch('rA', 'https://github.com/repoA_loc', 'foo'),
    ]).encode('utf-8'),

    '/repos/repo2_loc/pulls?state=all': b'[]',

    '/repos/repoA_loc/pulls?state=all':
    json.dumps([
        github_pullreq('https://github.com/repoA_loc', '2222',
                       'foo', 'rAprFooref', 'Foo Do',
                       'https://github.com/RepoA_prfoo_loc',
                       'bar', 10, state="merged"),
    ]).encode('utf-8'),

    '/users/bar': github_user('https://github.com/repo1_loc',
                              'bar', '10', 'bar@brown.cow'),

    '/repos/repo1_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/repo1_loc', 'master',
                               repo1_master_gitmodules),

    '/repos/Repo1_prfoo_loc/contents/.gitmodules?ref=foo':
    github_gitmodules_contents('https://github.com/repo1_loc', 'master',
                               repo1_foo_gitmodules),

    '/repos/repo2_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/repo2_loc', 'master',
                               repo2_master_gitmodules),

    '/repos/repo1_loc/contents/sub/repoA?ref=master':
    github_submodule_contents('https://github.com/repo1_loc', 'master',
                              'RepoA', 'sub/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

    '/repos/Repo1_prfoo_loc/contents/sub/repoA?ref=foo':
    github_submodule_contents('https://github.com/repo1_loc', 'foo',
                              'RepoA', 'sub/repoA',
                              'repoA_foo_head',
                               'https://github.com/repoA_loc'),

    '/repos/repo2_loc/contents/deps/repoA?ref=master':
    github_submodule_contents('https://github.com/repo2_loc', 'master',
                              'RepoA', 'deps/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

}


# #######################################################################


forge_stats = {

    'requests': {
        n:1 for n in
        [
            '/users/bar',
            '/repos/repoA_loc/branches',
            '/repos/repoA_loc/pulls?state=all',
            '/repos/repo2_loc/branches',
            '/repos/repo2_loc/pulls?state=all',
            '/repos/repo2_loc/contents/.gitmodules?ref=dog',
            '/repos/repo2_loc/contents/.gitmodules?ref=master',
            '/repos/repo2_loc/contents/deps/repoA?ref=master',
        ]
    },

    'responses': {
        n:[404] for n in [
            '/repos/repo2_loc/contents/.gitmodules?ref=dog',
        ]
    },

}
