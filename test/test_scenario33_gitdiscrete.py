from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import json
from test_scenario33 import (proj1_input_spec, expected_repo_proj1_info,
                             proj2_input_spec, expected_repo_proj2_info,
                             proj3_input_spec, expected_repo_proj3_info)
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents,
                       github_branch,
                       github_pullreq,
                       github_user)


fakeforge_port = 4533



@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo_proj1(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(proj1_input_spec,
                                                    actor_system=actor_system)
    assert expected_repo_proj1_info == repo_info


@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo_proj2(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(proj2_input_spec,
                                                    actor_system=actor_system)
    assert expected_repo_proj2_info == repo_info


@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo_proj3(get_hub_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_hub_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(proj3_input_spec,
                                                    actor_system=actor_system)
    assert expected_repo_proj3_info == repo_info


repo2_branches = json.dumps([
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
]).encode('utf-8')


repo2_pulls = json.dumps([
    github_pullreq('https://github.com/repo2_loc', '4444',
                       'master', 'r2prquuxref', 'Quux Do',
                       'https://github.com/Repo2_prquux_loc',
                       'zeb', 11),
]).encode('utf-8')


repo1_master_gitmodules = b'''
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
        { "name": "master",
          "commit": {
              "sha": "r1_master_ref",
              "url": "https://github.com/repo1_loc/commits/master",
          },
        },
    ]).encode('utf-8'),

    '/repos/repo2_loc/branches': repo2_branches,
    '/repos/repo2sub_loc/branches': repo2_branches,

    '/repos/repo3_loc/branches':
    json.dumps([
        { "name": "master",
          "commit": {
              "sha": "r3_master_ref",
              "url": "https://github.com/repo3_loc/commits/master",
          },
        },
        github_branch('rA', 'https://github.com/repoA_loc', 'foo'),
    ]).encode('utf-8'),

    '/repos/repoA_loc/branches':
    json.dumps([
        { "name": "master",
          "commit": {
              "sha": "rA_master_ref",
              "url": "https://github.com/repoA_loc/commits/master",
          },
        },
    ]).encode('utf-8'),

    # ------------------------------------------------------------

    '/repos/repo1_loc/pulls?state=all':
    json.dumps([
        github_pullreq('https://github.com/repo1_loc', '2222',
                       'master', 'r1prFooref', 'Foo Do',
                       'https://github.com/Repo1_prfoo_loc',
                       'bar', 10),
        github_pullreq('https://github.com/repo1_loc', '3333',
                       'master', 'r1prBarref', 'Bar Do Not',
                       'https://github.com/Repo1_prbar_loc',
                       'bar', 10),
    ]).encode('utf-8'),

    '/repos/repo2_loc/pulls?state=all': repo2_pulls,
    '/repos/repo2sub_loc/pulls?state=all': repo2_pulls,
    '/repos/repo3_loc/pulls?state=all': b'[]',
    '/repos/repoA_loc/pulls?state=all':
    json.dumps([
        github_pullreq('https://github.com/repoA_loc', '2200',
                       'master', 'rAprFooref', 'Foo Do 2',
                       'https://github.com/RepoA_prfoo_loc',
                       'bar', 10),
    ]).encode('utf-8'),

    # ------------------------------------------------------------

    '/users/bar': github_user('https://github.com/repo1_loc',
                              'bar', '10', 'bar@brown.cow'),

    '/users/zeb': github_user('https://github.com/repo2_loc',
                              'zeb', '11', 'zeb@barn.farm'),

    # ------------------------------------------------------------

    '/repos/repo1_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/repo1_loc', 'master',
                               repo1_master_gitmodules),

    '/repos/Repo1_prfoo_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/Repo1_prfoo_loc',
                               'master',
                               repo1_master_gitmodules),

    '/repos/Repo1_prbar_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/Repo1_prbar_loc',
                               'master',
                               repo1_master_gitmodules),

    # ....................

    '/repos/repo1_loc/contents/sub/repoA?ref=master':
    github_submodule_contents('https://github.com/repo1_loc', 'master',
                              'RepoA', 'sub/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

    '/repos/Repo1_prfoo_loc/contents/sub/repoA?ref=master':
    github_submodule_contents('https://github.com/Repo1_prfoo_loc',
                              'master',
                              'RepoA', 'sub/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

    '/repos/Repo1_prbar_loc/contents/sub/repoA?ref=master':
    github_submodule_contents('https://github.com/Repo1_prbar_loc',
                              'master',
                              'RepoA', 'sub/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

    # --------------------

    '/repos/repo2_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/repo2_loc', 'master',
                               repo2_master_gitmodules),

    '/repos/Repo2_prquux_loc/contents/.gitmodules?ref=master':
    github_gitmodules_contents('https://github.com/Repo2_prquux_loc',
                               'master',
                               repo2_master_gitmodules),


    # ....................

    '/repos/repo2_loc/contents/deps/repoA?ref=master':
    github_submodule_contents('https://github.com/repo2_loc', 'master',
                              'RepoA', 'deps/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

    '/repos/Repo2_prquux_loc/contents/deps/repoA?ref=master':
    github_submodule_contents('https://github.com/Repo2_prquux_loc',
                              'master',
                              'RepoA', 'deps/repoA',
                              'repoA_master_head',
                               'https://github.com/repoA_loc'),

}


# #######################################################################


forge_stats = {

    'requests': {
        n:(2 if n == '/users/bar' else 1) for n in [
            '/users/bar',
            '/users/zeb',
            '/repos/repoA_loc/branches',
            '/repos/repoA_loc/pulls?state=all',
            '/repos/repo1_loc/branches',
            '/repos/repo1_loc/pulls?state=all',
            '/repos/repo2_loc/branches',
            '/repos/repo2_loc/pulls?state=all',
            '/repos/repo3_loc/branches',
            '/repos/repo3_loc/pulls?state=all',
            '/repos/repo2_loc/contents/.gitmodules?ref=dog',
            '/repos/repo2_loc/contents/.gitmodules?ref=master',
            '/repos/repo2_loc/contents/deps/repoA?ref=master',
            '/repos/repo1_loc/contents/.gitmodules?ref=master',
            '/repos/repo1_loc/contents/sub/repoA?ref=master',
            '/repos/repo3_loc/contents/.gitmodules?ref=master',
            '/repos/Repo1_prbar_loc/contents/.gitmodules?ref=master',
            '/repos/Repo1_prbar_loc/contents/sub/repoA?ref=master',
            '/repos/Repo1_prfoo_loc/contents/.gitmodules?ref=master',
            '/repos/Repo1_prfoo_loc/contents/sub/repoA?ref=master',
            '/repos/Repo2_prquux_loc/contents/.gitmodules?ref=master',
            '/repos/Repo2_prquux_loc/contents/deps/repoA?ref=master',
        ]},

    'responses': {
        '/users/bar': [200, 200],
        '/repos/repo3_loc/contents/.gitmodules?ref=master': [404],
        '/repos/repo2_loc/contents/.gitmodules?ref=dog': [404],
    },

}
