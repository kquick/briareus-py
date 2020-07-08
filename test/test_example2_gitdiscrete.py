from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import json
import base64
from test_example2 import expected_repo_info
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       get_gitlab_api_url_local)


input_spec = open('test/inp_example2').read()

fakeforge_port = 4324


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




r1_gitmodules_master = base64.b64encode(b'''[submodule "r2_sub"]
	path = deps/r2
	url = https://github.com/r2_url
[submodule "r3-sub"]
	path = deps/r3
	url = https://gitlab.com/r3_url
[submodule "r4-sub"]
	path = deps/r4
	url = https://github.com/r4_url
''').decode('utf-8')


r1_gitmodules_develop = r1_gitmodules_master


# n.b. forge_responses also contains responses for test_example2_add_gitdiscrete

forge_responses = {

    ## R1 branches

    '/repos/r1_url/branches': b'''
[
  {
    "name": "develop",
    "commit": {
      "sha": "r1-develop-ref",
      "url": "https://github.com/r1_url/commits/r1-develop-ref"
    },
    "protected": false
  },
  {
    "name": "master",
    "commit": {
      "sha": "Repo1-master-ref",
      "url": "https://github.com/r1_url/commits/Repo1-master-ref"
    },
    "protected": false
  },
  {
    "name": "misc",
    "commit": {
      "sha": "r1-misc-ref",
      "url": "https://github.com/r1_url/commits/r1-misc-ref"
    },
    "protected": false
  },
  {
    "name": "stuff/here",
    "commit": {
      "sha": "r1-stuff/here-ref",
      "url": "https://github.com/r1_url/commits/r1-stuff/here-ref"
    },
    "protected": false
  }
]
    ''',

    ## R2 branches

    '/repos/r2_url/branches': b'''
[
  {
    "name": "develop",
    "commit": {
      "sha": "r2-develop-ref",
      "url": "https://github.com/r2_url/commits/r2-develop-ref"
    },
    "protected": false
  },
  {
    "name": "master",
    "commit": {
      "sha": "Repo2-master-ref",
      "url": "https://github.com/r2_url/commits/Repo2-master-ref"
    },
    "protected": false
  },
  {
    "name": "humdrum",
    "commit": {
      "sha": "r2-humdrum-ref",
      "url": "https://github.com/r2_url/commits/r2-humdrum-ref"
    },
    "protected": false
  }
]
    ''',

    ## R3 branches

    '/api/v4/projects/r3_url/repository/branches': b'''
[
  {
    "name": "master",
    "commit": {
      "id": "Repo3-master-ref",
      "short_id": "Repo3-master-shortref",
      "created_at": "2020-01-29T10:58:29.000-08:00",
      "parent_ids": null,
      "title": "Latest commit",
      "message": "Latest commit",
      "author_name": "Doctor Who",
      "author_email": "drwho@tardis.out",
      "authored_date": "2020-01-29T10:58:29.000-08:00",
      "committer_name": "Doctor Who",
      "committer_email": "drwho@tardis.out",
      "committed_date": "2020-01-29T10:58:29.000-08:00",
      "web_url": "https://gitlab.com/dalek/disabler/-/commit/Repo3-master-ref"
    },
    "merged": false,
    "protected": true,
    "developers_can_push": false,
    "developers_can_merge": false,
    "can_push": true,
    "default": true,
    "web_url": "https://gitlab.com/dalek/disabler/-/tree/master"
  },
  {
    "name": "develop",
    "commit": {
      "id": "r3-develop-ref",
      "short_id": "r3-develop-shortref",
      "created_at": "2019-10-24T15:54:00.000-07:00",
      "parent_ids": null,
      "title": "develop commit",
      "message": "develop commit",
      "author_name": "Doctor Who",
      "author_email": "drwho@tardis.out",
      "authored_date": "2019-10-24T15:54:00.000-07:00",
      "committer_name": "Doctor Who",
      "committer_email": "drwho@tardis.out",
      "committed_date": "2019-10-24T15:54:00.000-07:00",
      "web_url": "https://gitlab.com/dalek/disabler/-/commit/r3-develop-ref"
    },
    "merged": false,
    "protected": false,
    "developers_can_push": false,
    "developers_can_merge": false,
    "can_push": true,
    "default": false,
    "web_url": "https://gitlab.com/dalek/disabler/-/tree/develop"
  }
]
    ''',

    ## R4 branches

    '/repos/r4_url/branches': b'''
[
  {
    "name": "master",
    "commit": {
      "sha": "Repo4-master-ref",
      "url": "https://github.com/r4_url/commits/Repo4-master-ref"
    },
    "protected": false
  }
]
    ''',

    ## RAdd1 branches

    '/repos/ra1_url/branches': b'''
[
  {
    "name": "master",
    "commit": {
      "sha": "RAdd1-master-ref",
      "url": "https://github.com/ra1_url/commits/RAdd1-master-ref"
    },
    "protected": false
  }
]
    ''',

    ## RAdd2 branches

    '/repos/ra2_url/branches': b'''
[
  {
    "name": "master",
    "commit": {
      "sha": "RAdd2-master-ref",
      "url": "https://github.com/ra2_url/commits/RAdd2-master-ref"
    },
    "protected": false
  }
]
    ''',

    # ------------------------------------------------------------

    '/repos/r1_url/pulls?state=all': b'[]',
    '/repos/r2_url/pulls?state=all': b'[]',
    '/api/v4/projects/r3_url/merge_requests?scope=all&state=all': b'[]',
    '/repos/r4_url/pulls?state=all': b'[]',
    '/repos/ra1_url/pulls?state=all': b'[]',
    '/repos/ra2_url/pulls?state=all': b'[]',

    # ------------------------------------------------------------

    '/repos/r1_url/contents/.gitmodules?ref=master' : json.dumps(
        {
            "name": ".gitmodules",
            "path": ".gitmodules",
            "sha": "9ec4b3415097850f9f9ec3fa50cb03367c473c24",
            "size": len(r1_gitmodules_master),
            "url": "https://api.github.com/repos/r1_repo/contents/.gitmodules?ref=master",
            "html_url": "https://github.com/r1_repo/blob/master/.gitmodules",
            "git_url": "https://api.github.com/repos/r1_repo/git/blobs/9ec4b3415097850f9f9ec3fa50cb03367c473c24",
            "download_url": "https://raw.githubusercontent.com/r1_repo/master/.gitmodules",
            "type": "file",
            "encoding": "base64",
            "content": r1_gitmodules_master,
        }).encode('utf-8'),

    '/repos/r1_url/contents/.gitmodules?ref=develop' : json.dumps(
        {
            "name": ".gitmodules",
            "path": ".gitmodules",
            "sha": "9ec4b3415097850f9f9ec3fa50cb03367c473c25",
            "size": len(r1_gitmodules_develop),
            "url": "https://api.github.com/repos/r1_repo/contents/.gitmodules?ref=develop",
            "html_url": "https://github.com/r1_repo/blob/develop/.gitmodules",
            "git_url": "https://api.github.com/repos/r1_repo/git/blobs/9ec4b3415097850f9f9ec3fa50cb03367c473c25",
            "download_url": "https://raw.githubusercontent.com/r1_repo/develop/.gitmodules",
            "type": "file",
            "encoding": "base64",
            "content": r1_gitmodules_develop,
        }).encode('utf-8'),

    ## deps/r2

    '/repos/r1_url/contents/deps/r2?ref=master' : b'''
{
  "name": "Repo2",
  "path": "deps/r2_sub",
  "sha": "r2_master_head",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r2_sub?ref=master",
  "html_url": "https://github.com/r2_url/tree/r2_master_head",
  "git_url": "https://api.github.com/repos/r2_url/git/trees/r2_master_head",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://github.com/r2_url"
}
    ''',


    '/repos/r1_url/contents/deps/r2?ref=develop' : b'''
{
  "name": "Repo2",
  "path": "deps/r2_sub",
  "sha": "r2_develop_head",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r2_sub?ref=master",
  "html_url": "https://github.com/r2_url/tree/r2_develop_head",
  "git_url": "https://api.github.com/repos/r2_url/git/trees/r2_develop_head",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://github.com/r2_url"
}
    ''',


    ## deps/r3

    '/repos/r1_url/contents/deps/r3?ref=master' : b'''
{
  "name": "Repo3",
  "path": "deps/r3-sub",
  "sha": "r3_master_head^3",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r3?ref=master",
  "html_url": "https://gitlab.com/r3_url/tree/r3_master_head^3",
  "git_url": "https://api.gitlab.com/repos/r3_url/git/trees/r3_master_head^3",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://gitlab.com/r3_url"
}
    ''',

    '/repos/r1_url/contents/deps/r3?ref=develop' : b'''
{
  "name": "Repo3",
  "path": "deps/r3-sub",
  "sha": "r3_develop_head",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r3?ref=develop",
  "html_url": "https://gitlab.com/r3_url/tree/r3_develop_head",
  "git_url": "https://api.gitlab.com/repos/r3_url/git/trees/r3_develop_head",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://gitlab.com/r3_url"
}
    ''',

    ## deps/r4

    '/repos/r1_url/contents/deps/r4?ref=master' : b'''
{
  "name": "Repo4",
  "path": "deps/r4-sub",
  "sha": "r4_master_head^1",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r4?ref=master",
  "html_url": "https://github.com/r4_url/tree/r4_master_head^1",
  "git_url": "https://api.github.com/repos/r4_url/git/trees/r4_master_head^1",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://github.com/r4_url"
}
    ''',

    '/repos/r1_url/contents/deps/r4?ref=develop' : b'''
{
  "name": "Repo4",
  "path": "deps/r4-sub",
  "sha": "r4_master_head",
  "size": 0,
  "url": "https://api.github.com/repos/r1_url/contents/deps/r4?ref=develop",
  "html_url": "https://github.com/r4_url/tree/r4_master_head",
  "git_url": "https://api.github.com/repos/r4_url/git/trees/r4_master_head",
  "download_url": null,
  "type": "submodule",
  "submodule_git_url": "https://github.com/r4_url"
}
    ''',

    # ------------------------------------------------------------

    '/repos/r1_url' : b'''
{
  "id": 2431,
  "node_id": "MDEwOlJlchhhaaavcnkyNDMxMjYxMTg=",
  "name": "Repo1",
  "full_name": "me/Repo1",
  "private": false,
  "owner": {
    "login": "me",
    "id": 159,
    "node_id": "MDEyOk9yZ2FuaXphdGlvbjE1hohoNzQ=",
    "type": "Organization",
    "site_admin": false
  },
  "html_url": "https://github.com/r1_url",
  "description": "Repo 1 here",
  "fork": false,
  "url": "https://api.github.com/repos/r1_url",
  "created_at": "2020-02-25T23:38:15Z",
  "updated_at": "2020-06-05T04:43:18Z",
  "pushed_at": "2020-06-13T00:16:38Z",
  "homepage": null,
  "size": 1543,
  "stargazers_count": 25,
  "watchers_count": 25,
  "language": "C",
  "has_issues": true,
  "has_projects": true,
  "has_downloads": true,
  "has_wiki": true,
  "has_pages": false,
  "forks_count": 2,
  "mirror_url": null,
  "archived": false,
  "disabled": false,
  "open_issues_count": 18,
  "license": null,
  "forks": 2,
  "open_issues": 18,
  "watchers": 25,
  "default_branch": "master",
  "temp_clone_token": null,
  "organization": {
    "login": "me",
    "id": 159,
    "type": "Organization",
    "site_admin": false
  },
  "network_count": 2,
  "subscribers_count": 27
}
    ''',

}


# #######################################################################


forge_stats = {

    'requests': {
        n:1 for n in
        [
            '/repos/r1_url/branches',
            '/repos/r1_url/pulls?state=all',
            '/repos/r2_url/branches',
            '/repos/r2_url/pulls?state=all',
            '/api/v4/projects/r3_url/repository/branches',
            '/api/v4/projects/r3_url/merge_requests?scope=all&state=all',
            '/repos/r4_url/branches',
            '/repos/r4_url/pulls?state=all',
            '/repos/r1_url/contents/.gitmodules?ref=master',
            '/repos/r1_url/contents/.gitmodules?ref=develop',
            '/repos/r1_url/contents/deps/r2?ref=master',
            '/repos/r1_url/contents/deps/r2?ref=develop',
            '/repos/r1_url/contents/deps/r3?ref=master',
            '/repos/r1_url/contents/deps/r3?ref=develop',
            '/repos/r1_url/contents/deps/r4?ref=master',
            '/repos/r1_url/contents/deps/r4?ref=develop',
        ]
    },

    'responses': {},

}
