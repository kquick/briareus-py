from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
from test_single import expected_repo_info
import pytest
from unittest.mock import patch
from FakeForge import fake_forge, get_github_api_url_local


input_spec = open('test/inp_single').read()

fakeforge_port = 4234

@patch.object(Briareus.VCS.GitForge.GitHubInfo, 'get_api_url')
def test_gitinfo(get_api_url, actor_system, fake_forge):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    get_api_url.side_effect = get_github_api_url_local(fakeforge_port)

    input_desc, repo_info = input_desc_and_VCS_info(input_spec, actor_system=actor_system)
    assert repo_info == expected_repo_info


forge_responses = {
    '/repos/the_repo_url/branches': b'''
[
  {
    "name": "master",
    "commit": {
      "sha": "master-ref",
      "url": "https://github.com/the_repo_url/commits/master-ref"
    },
    "protected": false
  },
  {
    "name": "feat1",
    "commit": {
      "sha": "feat1-ref",
      "url": "https://github.com/the_repo_url/commits/feat1-ref"
    },
    "protected": false
  }
]
    ''',


    '/repos/the_repo_url/pulls?state=all': b'''
[
  {
    "url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134",
    "id": 365427239,
    "node_id": "MDExOlB1bGxSZXF1ZXN0MzY1NDI3MjM5",
    "html_url": "https://localhost/the_repo_url/pull/134",
    "diff_url": "https://localhost/the_repo_url/pull/134.diff",
    "patch_url": "https://localhost/the_repo_url/pull/134.patch",
    "issue_url": "https://github.com/the_repo_url/repos/the_repo_url/issues/134",
    "number": 134,
    "state": "open",
    "locked": false,
    "title": "Hoppy toads",
    "user": {
      "login": "hoppy",
      "id": 555,
      "type": "User",
      "site_admin": false
    },
    "body": "",
    "created_at": "2020-01-21T17:35:14Z",
    "updated_at": "2020-01-26T19:35:44Z",
    "closed_at": null,
    "merged_at": null,
    "merge_commit_sha": "toad_merge_commit_ref",
    "assignee": null,
    "assignees": [

    ],
    "requested_reviewers": [

    ],
    "requested_teams": [

    ],
    "labels": [

    ],
    "milestone": null,
    "draft": false,
    "commits_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134/commits",
    "review_comments_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134/comments",
    "review_comment_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/comments{/number}",
    "comments_url": "https://github.com/the_repo_url/repos/the_repo_url/issues/134/comments",
    "statuses_url": "https://github.com/the_repo_url/repos/the_repo_url/statuses/toad_mergeref",
    "head": {
      "label": "toad_repo:toad",
      "ref": "toad",
      "sha": "toad_mergeref",
      "user": {
        "login": "Mr. Toad",
        "id": 555,
        "type": "User",
        "site_admin": false
      },
      "repo": {
        "id": 235401723,
        "name": "the_repo",
        "full_name": "proj_name/the_repo",
        "private": false,
        "owner": {
          "login": "Mr. Toad",
          "id": 555,
          "type": "User",
          "site_admin": false
        },
        "html_url": "https://github.com/toad_repo_url",
        "description": "The Repo",
        "fork": false,
        "created_at": "2020-01-21T17:32:49Z",
        "updated_at": "2020-01-21T17:37:42Z",
        "pushed_at": "2020-01-26T19:35:45Z",
        "homepage": null,
        "size": 3,
        "stargazers_count": 0,
        "watchers_count": 0,
        "language": null,
        "has_issues": false,
        "has_projects": true,
        "has_downloads": true,
        "has_wiki": true,
        "has_pages": false,
        "forks_count": 0,
        "mirror_url": null,
        "archived": false,
        "disabled": false,
        "open_issues_count": 1,
        "license": null,
        "forks": 0,
        "open_issues": 1,
        "watchers": 0,
        "default_branch": "master"
      }
    },
    "base": {
      "label": "the_repo:master",
      "ref": "master",
      "sha": "toad_master_ref",
      "user": {
        "login": "Mr. Toad",
        "id": 555,
        "type": "User",
        "site_admin": false
      },
      "repo": {
        "id": 235401723,
        "name": "the_repo",
        "full_name": "proj_name/the_repo",
        "private": false,
        "owner": {
          "login": "Mr. Toad",
          "id": 555,
          "type": "User",
          "site_admin": false
        },
        "html_url": "https://localhost/the_repo_url",
        "description": "The Repo",
        "fork": false,
        "url": "https://github.com/the_repo_url",
        "created_at": "2020-01-21T17:32:49Z",
        "updated_at": "2020-01-21T17:37:42Z",
        "pushed_at": "2020-01-26T19:35:45Z",
        "homepage": null,
        "size": 3,
        "stargazers_count": 0,
        "watchers_count": 0,
        "language": null,
        "has_issues": false,
        "has_projects": true,
        "has_downloads": true,
        "has_wiki": true,
        "has_pages": false,
        "forks_count": 0,
        "mirror_url": null,
        "archived": false,
        "disabled": false,
        "open_issues_count": 1,
        "license": null,
        "forks": 0,
        "open_issues": 1,
        "watchers": 0,
        "default_branch": "master"
      }
    },
    "_links": {
      "self": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134"
      },
      "html": {
        "href": "https://localhost/the_repo_url/pull/134"
      },
      "issue": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/issues/134"
      },
      "comments": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/issues/134/comments"
      },
      "review_comments": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134/comments"
      },
      "review_comment": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/comments{/number}"
      },
      "commits": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/134/commits"
      },
      "statuses": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/statuses/toad_mergeref"
      }
    },
    "author_association": "OWNER",
    "active_lock_reason": null
  },

  {
    "url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91",
    "id": 365427238,
    "node_id": "MDExOlB1bGxSZXF1ZXN0MzY1NDI3MjM5",
    "html_url": "https://localhost/the_repo_url/pull/91",
    "diff_url": "https://localhost/the_repo_url/pull/91.diff",
    "patch_url": "https://localhost/the_repo_url/pull/91.patch",
    "issue_url": "https://github.com/the_repo_url/repos/the_repo_url/issues/91",
    "number": 91,
    "state": "open",
    "locked": false,
    "title": "Croaking frogs",
    "user": {
      "login": "frog",
      "id": 554,
      "type": "User",
      "site_admin": false
    },
    "body": "",
    "created_at": "2020-01-21T17:35:14Z",
    "updated_at": "2020-01-26T19:35:44Z",
    "closed_at": null,
    "merged_at": null,
    "merge_commit_sha": "frog_merge_commit_ref",
    "assignee": null,
    "assignees": [

    ],
    "requested_reviewers": [

    ],
    "requested_teams": [

    ],
    "labels": [

    ],
    "milestone": null,
    "draft": false,
    "commits_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91/commits",
    "review_comments_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91/comments",
    "review_comment_url": "https://github.com/the_repo_url/repos/the_repo_url/pulls/comments{/number}",
    "comments_url": "https://github.com/the_repo_url/repos/the_repo_url/issues/91/comments",
    "statuses_url": "https://github.com/the_repo_url/repos/the_repo_url/statuses/toad_mergeref",
    "head": {
      "label": "frog_repo:frog",
      "ref": "frog",
      "sha": "frog_mergeref",
      "user": {
        "login": "frog",
        "id": 554,
        "type": "User",
        "site_admin": false
      },
      "repo": {
        "id": 235401723,
        "name": "the_repo",
        "full_name": "proj_name/the_repo",
        "private": false,
        "owner": {
          "login": "Mr. Toad",
          "id": 555,
          "type": "User",
          "site_admin": false
        },
        "html_url": "https://github.com/frog_repo_url",
        "description": "The Repo",
        "fork": false,
        "created_at": "2020-01-21T17:32:49Z",
        "updated_at": "2020-01-21T17:37:42Z",
        "pushed_at": "2020-01-26T19:35:45Z",
        "homepage": null,
        "size": 3,
        "stargazers_count": 0,
        "watchers_count": 0,
        "language": null,
        "has_issues": false,
        "has_projects": true,
        "has_downloads": true,
        "has_wiki": true,
        "has_pages": false,
        "forks_count": 0,
        "mirror_url": null,
        "archived": false,
        "disabled": false,
        "open_issues_count": 1,
        "license": null,
        "forks": 0,
        "open_issues": 1,
        "watchers": 0,
        "default_branch": "master"
      }
    },
    "base": {
      "label": "the_repo:master",
      "ref": "master",
      "sha": "frog_master_ref",
      "user": {
        "login": "Mr. Toad",
        "id": 555,
        "type": "User",
        "site_admin": false
      },
      "repo": {
        "id": 235401723,
        "name": "the_repo",
        "full_name": "proj_name/the_repo",
        "private": false,
        "owner": {
          "login": "Mr. Toad",
          "id": 555,
          "type": "User",
          "site_admin": false
        },
        "html_url": "https://localhost/the_repo_url",
        "description": "The Repo",
        "fork": false,
        "url": "https://github.com/the_repo_url",
        "created_at": "2020-01-21T17:32:49Z",
        "updated_at": "2020-01-21T17:37:42Z",
        "pushed_at": "2020-01-26T19:35:45Z",
        "homepage": null,
        "size": 3,
        "stargazers_count": 0,
        "watchers_count": 0,
        "language": null,
        "has_issues": false,
        "has_projects": true,
        "has_downloads": true,
        "has_wiki": true,
        "has_pages": false,
        "forks_count": 0,
        "mirror_url": null,
        "archived": false,
        "disabled": false,
        "open_issues_count": 1,
        "license": null,
        "forks": 0,
        "open_issues": 1,
        "watchers": 0,
        "default_branch": "master"
      }
    },
    "_links": {
      "self": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91"
      },
      "html": {
        "href": "https://localhost/the_repo_url/pull/91"
      },
      "issue": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/issues/91"
      },
      "comments": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/issues/91/comments"
      },
      "review_comments": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91/comments"
      },
      "review_comment": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/comments{/number}"
      },
      "commits": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/pulls/91/commits"
      },
      "statuses": {
        "href": "https://github.com/the_repo_url/repos/the_repo_url/statuses/toad_mergeref"
      }
    },
    "author_association": "OWNER",
    "active_lock_reason": null
  }

]

    ''',


    '/users/hoppy' : b'''
{
  "login": "hoppy",
  "id": 543,
  "node_id": "MDQ555Nlcjc4NzQyMQ==",
  "url": "https://github.com/the_repo_url/users/hoppy",
  "html_url": "https://localhost/hoppy",
  "type": "User",
  "site_admin": false,
  "name": "Hoppy T.",
  "company": "@rock ",
  "blog": "",
  "location": "Desert, Dusty",
  "email": null,
  "hireable": null,
  "bio": null,
  "twitter_username": null,
  "public_repos": 54,
  "public_gists": 22,
  "followers": 24,
  "following": 3,
  "created_at": "2011-05-14T06:20:56Z",
  "updated_at": "2020-05-21T14:26:57Z"
}

    ''',

    '/users/frog' : b'''
{
  "login": "frog",
  "id": 542,
  "node_id": "MDQ5555lcjc4NzQyMQ==",
  "url": "https://github.com/the_repo_url/users/frog",
  "html_url": "https://localhost/frog",
  "type": "User",
  "site_admin": false,
  "name": "Frog G.",
  "company": "@lilypond.pad ",
  "blog": "",
  "location": "Pond, Water",
  "email": "frog@lilypond.pad",
  "hireable": null,
  "bio": null,
  "twitter_username": null,
  "public_repos": 55,
  "public_gists": 23,
  "followers": 25,
  "following": 4,
  "created_at": "2012-05-15T06:20:56Z",
  "updated_at": "2019-05-20T14:26:57Z"
}

    ''',

    # '/repos/the_repo_url/contents/.gitmodules?ref=master' : b'''
    # ''',
}
