from Briareus.Input.Operations import input_desc_and_VCS_info
import Briareus.VCS.GitForge
import base64
import json
from test_exampledups import expected_repo_info
import pytest
from unittest.mock import patch
from FakeForge import (fake_forge, get_github_api_url_local,
                       github_gitmodules_contents,
                       github_submodule_contents,
                       github_branch,
                       gitlab_branch,
                       github_pullreq,
                       github_user)


input_spec = open('test/inp_exampledups').read()


fakeforge_port = 4344


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

    '/repos/r1_url/branches':
    json.dumps([
        github_branch('Repo1', 'https://github.com/r1_url', 'master'),
        github_branch('r1', 'https://github.com/r1_url', 'develop'),
    ]).encode('utf-8'),

    '/repos/r2_url/branches':
    json.dumps([
        github_branch('Repo2', 'https://github.com/r2_url', 'master'),
        github_branch('r2', 'https://github.com/r2_url', 'develop'),
        github_branch('r2', 'https://github.com/r2_url', 'foo'),
    ]).encode('utf-8'),

    '/repos/r3_url/branches':
    json.dumps([
        github_branch('Repo3', 'https://github.com/r3_url', 'master'),
        github_branch('r3', 'https://github.com/r3_url', 'develop'),
    ]).encode('utf-8'),

    '/repos/r1_url/pulls?state=all':
    json.dumps([
        # This PR uses "master", which is the same thing as the main
        # branch.  This should co-exist with the master build.
        github_pullreq('https://github.com/r1_url', '1',
                       'master', 'r1_master_maskref', 'pr#mastermask',
                       'https://github.com/remote_Repo1',
                       'jdoe', 1010),
        # This PR should be built with the corresponding dog PR in
        # Repo3
        github_pullreq('https://github.com/r1_url', '8',
                       'dog', 'r1_r8_f32', 'pr8 is great',
                       'https://github.com/Repo1_Remote8',
                       'r.user', 1011),
        # This PR *also* uses "master".  It should be built distinctly
        # from PR1, co-exist with the "master" build, and not involve
        # PR9 from R3.  The PR number also matches a PR in Repo3, but
        # this also should not cause confusion
        github_pullreq('https://github.com/r1_url', '2',
                       'master', 'r1_master_p2^head', 'pr numero dos',
                       'https://github.com/remote_Repo1_pr2',
                       'jdoe', 1010),
    ]).encode('utf-8'),


    '/repos/r2_url/pulls?state=all': b'[]',


    '/repos/r3_url/pulls?state=all':
    json.dumps([
        # This PR is for develop, but it should co-exist with the
        # develop branch in R3; R1 and R2 develop should be built
        # against the R3 develop branch and this PR.
        github_pullreq('https://github.com/r3_url', '2',
                       'develop', 'r3_develop_pr2', 'pr#develop',
                       'https://github.com/remote_Repo3',
                       'frank', 1012),
        # This PR has a corresponding branch in R2 it should be built
        # against.  Note also that it duplicates the ID from the Repo1
        # PR; verify that these don't get confused/combined.
        github_pullreq('https://github.com/r3_url', '1',
                       'foo', 'r3_foo_pr3', 'pr#foo',
                       'https://github.com/remote_Repo3_2',
                       'earl', 1013),
        # This PR is on master in the source repo, but because master
        # is the default branch, it should *not* be built with other
        # PR's on similar branches (notably Repo1, PR1).
        github_pullreq('https://github.com/r3_url', '9',
                       'master', 'r3_master_2', 'pr#master3',
                       'https://github.com/remote_repo3_other',
                       'frank', 1012),
        # This PR should be built with the corresponding dog PR in Repo1
        github_pullreq('https://github.com/r3_url', '101',
                       'dog', 'r3_r3^7', 'start changes',
                       'https://github.com/Repo3_r3',
                       'fido', 1014),
    ]).encode('utf-8'),

    '/users/jdoe': github_user('https://github.com/r1_url',
                               'jdoe', 1010, 'jdoe@nocompany.com'),

    '/users/r.user': github_user('https://github.com/r1_url', 'r.user', 1011),

    '/users/frank': github_user('https://github.com/r3_url',
                                'frank', 1012, 'frank@stein.co'),

    '/users/earl': github_user('https://github.com/r3_url',
                               'earl', 1013, 'earl@king.wild'),

    '/users/fido': github_user('https://github.com/r3_url',
                               'fido', 1014, 'fido@woof.grr'),

}
