from Briareus.Types import (BldConfig, BldRepoRev, BldVariable, BranchReq,
                            ProjectSummary, StatusReport, VarFailure)
from Briareus.Input.Operations import input_desc_and_VCS_info
from git_example2 import GitExample2
from test_example2_add import expected_repo_info
import json
import pytest
from datetime import timedelta


gitactor = GitExample2
input_spec = open('test/inp_example2_add').read()


def test_gitinfo(actor_system):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    gitinfo = actor_system.createActor(gitactor, globalName="GetGitInfo")
    input_desc, repo_info = input_desc_and_VCS_info(input_spec, actor_system=actor_system)
    assert repo_info == expected_repo_info
