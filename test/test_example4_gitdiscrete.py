from Briareus.VCS.InternalMessages import PullReqInfo, PRSts_Active
from Briareus.Input.Operations import input_desc_and_VCS_info
from git_example1 import GitExample1
from test_example4 import expected_repo_info
import pytest
from datetime import timedelta


gitactor = GitExample1
gitactor_updates = [
    ( ("primary branch", "R10", "develop"), "ok: R10 main branch is develop"),
    ( ("primary branch", "R4",  "primary"), "ok: R4 main branch is primary"),
]
input_spec = open('test/inp_example4').read()


def test_gitinfo(actor_system):
    """Tests to ensure that the GetGitInfo discrete REST queries actor
       returned information is translated into the proper repo
       information for Briareus to use.
    """
    gitinfo = actor_system.createActor(gitactor, globalName="GetGitInfo")
    for req,resp in gitactor_updates:
        r = actor_system.ask(gitinfo, req, timedelta(seconds=1))
        assert r == resp
    input_desc, repo_info = input_desc_and_VCS_info(input_spec, actor_system=actor_system)
    assert repo_info == expected_repo_info
