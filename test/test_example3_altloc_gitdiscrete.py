from Briareus.VCS.InternalMessages import PullReqInfo, PRSts_Active
from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import copy
from datetime import timedelta
from test_example3_altloc import expected_repo_info
import test_example1_gitdiscrete
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       get_gitlab_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents)


fakeforge_port = 4325


forge_responses = copy.deepcopy(test_example1_gitdiscrete.forge_responses)
forge_responses['/api/v4/projects/r10_url/merge_requests?scope=all&state=all'] = b'''
[
{
    "id": 355,
    "iid": 321,
    "project_id": 413,
    "title": "test R10 PR",
    "description": "",
    "state": "opened",
    "created_at": "2020-02-05T04:49:11.581Z",
    "updated_at": "2020-02-05T04:49:11.581Z",
    "merged_by": null,
    "merged_at": null,
    "closed_by": null,
    "closed_at": null,
    "target_branch": "master",
    "source_branch": "devtest",
    "user_notes_count": 0,
    "upvotes": 0,
    "downvotes": 0,
    "assignee": null,
    "author": {
      "id": 64,
      "name": "Miles",
      "username": "miles",
      "state": "active",
      "avatar_url": "https://gitlab.com/uploads/-/system/user/avatar/64/avatar.png",
      "web_url": "https://gitlab-int.galois.com/miles"
    },
    "assignees": [],
    "source_project_id": 413,
    "target_project_id": 413,
    "labels": [],
    "work_in_progress": false,
    "milestone": null,
    "merge_when_pipeline_succeeds": false,
    "merge_status": "can_be_merged",
    "sha": "r10_devtest_ref",
    "merge_commit_sha": null,
    "squash_commit_sha": null,
    "discussion_locked": null,
    "should_remove_source_branch": null,
    "force_remove_source_branch": false,
    "reference": "!2",
    "references": {
      "short": "!2",
      "relative": "!2",
      "full": "R10!2"
    },
    "web_url": "https://gitlab.com/r10_xlated_url/-/pullpath/part",
    "time_stats": {
      "time_estimate": 0,
      "total_time_spent": 0,
      "human_time_estimate": null,
      "human_total_time_spent": null
    },
    "squash": false,
    "task_completion_status": {
      "count": 0,
      "completed_count": 0
    },
    "has_conflicts": false,
    "blocking_discussions_resolved": true,
    "approvals_before_merge": null
  }
]
'''

forge_responses['/api/v4/users/64'] = b'''
{
  "id": 64,
  "name": "Miles",
  "username": "miles",
  "state": "active",
  "avatar_url": "https://gitlab.com/uploads/-/system/user/avatar/64/avatar.png",
  "web_url": "https://gitlab.com/miles",
  "created_at": "2017-04-11T01:02:29.401Z",
  "bio": "",
  "location": "",
  "public_email": "miles@to.go",
  "skype": "",
  "linkedin": "",
  "twitter": "",
  "website_url": "",
  "organization": "to",
  "job_title": "",
  "work_information": "to go"
}
'''


input_spec = open('test/inp_example3_altloc').read()


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
