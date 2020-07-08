from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import json
import base64
from test_example2_add import expected_repo_info
from datetime import timedelta
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       get_gitlab_api_url_local)
from test_example2_gitdiscrete import fakeforge_port, forge_responses


input_spec = open('test/inp_example2_add').read()


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
