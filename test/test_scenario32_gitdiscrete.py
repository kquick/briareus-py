from Briareus.Input.Operations import input_desc_and_VCS_info
from git_scenario32 import GitScenario32
from test_scenario32 import (proj1_input_spec, expected_repo_proj1_info,
                             proj2_input_spec, expected_repo_proj2_info)
import pytest


gitactor = GitScenario32


def test_gitinfo_proj1(actor_system):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    gitinfo = actor_system.createActor(gitactor, globalName="GetGitInfo")
    input_desc, repo_info = input_desc_and_VCS_info(proj1_input_spec,
                                                    actor_system=actor_system)
    assert repo_info == expected_repo_proj1_info

def test_gitinfo_proj2(actor_system):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    gitinfo = actor_system.createActor(gitactor, globalName="GetGitInfo")
    input_desc, repo_info = input_desc_and_VCS_info(proj2_input_spec,
                                                    actor_system=actor_system)
    assert repo_info == expected_repo_proj2_info
