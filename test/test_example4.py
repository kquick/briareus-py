from Briareus.Types import BldConfig, BldRepoRev, BldVariable, PR_Grouped, BranchReq, MainBranch
from Briareus.VCS_API import BranchRef, PRSts_Active, PRSts_Merged, PRSts_Closed
from Briareus.VCS.InternalMessages import (PRInfo, RepoDesc, SubModuleInfo)
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import json
import pytest
import test_example3 as tex

# Similar to test_example3 except:
#
#  * R10 does *not* have a "master" branch as its primary branch; the
#    primary branch for R10 is "develop".
#
#  * R4 uses "primary" as its primary branch, although it does also have
#    a master branch; this master branch should be ignored.
#
#  * None of these branches ("master", "primary", or "develop") is
#    listed in the "Branches" portion of the input; they should all be
#    inferred.
#
# The other repos (R3) still use "master".  The R3 "master" should be
# aligned with R4 "primary" and R10 "develop".

input_spec = open('test/inp_example4').read()


expected_repo_info = {
    'branches' : set([
        BranchRef(reponame='R10', branchname='develop', branchref='R10-master-ref'),
        BranchRef(reponame='R3', branchname='blah', branchref='r3-blah-ref'),
        BranchRef(reponame='R3', branchname='master', branchref='R3-master-ref'),
        BranchRef(reponame='R4', branchname='feat1', branchref='r4-feat1-ref'),
        BranchRef(reponame='R4', branchname='primary', branchref='R4-primary-ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='https://github.com/remote_r3_CLOSED_url', pr_branch='closed_pr',
               pr_revision='r3_CLOSED_mergeref', pr_ident='22', pr_status=PRSts_Closed(),
               pr_title='ignored because closed', pr_user='done', pr_email='done@already.yo'),
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='https://github.com/remote_r3_MERGED_url', pr_branch='merged_pr',
               pr_revision='r3_MERGED_mergeref', pr_ident='33', pr_status=PRSts_Merged(),
               pr_title='ignored because merged', pr_user='done', pr_email='done@already.yo'),
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='https://github.com/remote_r3_pr11_url', pr_branch='blah',
               pr_revision='r3_blah_mergeref', pr_ident='11', pr_status=PRSts_Active(),
               pr_title='blah started', pr_user='nick', pr_email='nick@bad.seeds'),
        PRInfo(pr_target_repo='R4', pr_srcrepo_url='https://github.com/remote_R4_y', pr_branch='bugfix9',
               pr_revision='r4_bf9_mergeref', pr_ident='8192', pr_status=PRSts_Active(),
               pr_title='fix ninth bug!', pr_user='ozzie', pr_email='ozzie@crazy.train'),
    ]),
    'submodules': set([
        SubModuleInfo(sm_repo_name='R10', sm_branch='develop', sm_pullreq_id=None, sm_sub_name='R3', sm_sub_vers='r3_master_head^9'),
        SubModuleInfo(sm_repo_name='R10', sm_branch='develop', sm_pullreq_id=None, sm_sub_name='R4', sm_sub_vers='r4_master_head^1'),
    ]),
    'subrepos': set([
        RepoDesc(repo_name='R3', repo_url='https://github.com/r3_url', main_branch='master', project_repo=False),
        RepoDesc(repo_name='R4', repo_url='https://github.com/r4_explicit_default_url', main_branch='primary', project_repo=False),
    ]),
}


@pytest.fixture(scope="module")
def example_hydra_jobsets(generated_hydra_builder_output):
    return generated_hydra_builder_output[0][None]

GS = [ "ghc822", "ghc844" ]
top_level = [
    "regular develop heads",
    "regular develop submodules",
    "regular feat1 heads",
    "regular dev heads",
    "pullreq bugfix9 heads",
    "pullreq bugfix9 submodules",
    "pullreq blah heads",
    "pullreq blah submodules",
]

expected_facts = sorted(filter(None, '''
:- discontiguous repo/2.
:- discontiguous project/1.
:- discontiguous project/2.
:- discontiguous main_branch/2.
:- discontiguous branchreq/2.
:- discontiguous subrepo/2.
:- discontiguous pullreq/7.
:- discontiguous branch/2.
:- discontiguous branch_ref/3.
:- discontiguous submodule/5.
:- discontiguous varname/2.
:- discontiguous varvalue/4.
project("R10").
project("R10", "R10").
repo("R10", "R10").
repo("R10", "R4").
default_main_branch("master").
main_branch("R10", "develop").
main_branch("R4", "primary").
subrepo("R10", "R3").
subrepo("R10", "R4").
branchreq("R10", "dev").
branchreq("R10", "feat1").
branch("R4", "primary").
branch("R3", "blah").
branch("R4", "feat1").
branch("R3", "master").
branch("R10", "develop").
branch_ref("R4", "primary", "R4-primary-ref").
branch_ref("R3", "blah", "r3-blah-ref").
branch_ref("R4", "feat1", "r4-feat1-ref").
branch_ref("R3", "master", "R3-master-ref").
branch_ref("R10", "develop", "R10-master-ref").
pullreq("R4", "8192", "bugfix9", "r4_bf9_mergeref", prsts_active, "ozzie", "ozzie@crazy.train").
pullreq("R3", "11", "blah", "r3_blah_mergeref", prsts_active, "nick", "nick@bad.seeds").
pullreq("R3", "22", "closed_pr", "r3_CLOSED_mergeref", prsts_closed, "done", "done@already.yo").
pullreq("R3", "33", "merged_pr", "r3_MERGED_mergeref", prsts_merged, "done", "done@already.yo").
submodule("R10", project_primary, "develop", "R4", "r4_master_head^1").
submodule("R10", project_primary, "develop", "R3", "r3_master_head^9").
varname("R10", "ghcver").
varvalue("R10", "ghcver", "ghc822", 0).
varvalue("R10", "ghcver", "ghc844", 1).
'''.split('\n')))

def test_example_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))


def test_example_internal_count(generated_bldconfigs):
    print('### bldcfgs:')
    for each in generated_bldconfigs.cfg_build_configs:
        print(each.projectname, each.branchtype, each.branchname, each.strategy)
    assert len(GS) * len(top_level) == len(generated_bldconfigs.cfg_build_configs)

def test_example_internal_no_blah_regular_submods(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "blah" and
                    each.strategy == "submodules")

def test_example_internal_no_blah_regular_HEADs(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "blah" and
                    each.strategy == "HEADs")


def test_example_internal_bugfix9_pullreq_submods(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="bugfix9",
                            strategy="submodules",
                            description=PR_Grouped("bugfix9"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "r3_master_head^9", "project_primary"),
                                  BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_bugfix9_pullreq_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="bugfix9",
                            strategy="HEADs",
                            description=PR_Grouped("bugfix9"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_no_bugfix9_regular_submods(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "bugfix9" and
                    each.strategy == "submodules")

def test_example_internal_no_bugfix9_regular_HEADs(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "bugfix9" and
                    each.strategy == "HEADs")

def test_example_internal_no_feat1_regular_submodules(generated_bldconfigs):
    # Because feat1 is not a branch in R10, and all other repos are
    # submodules, there is no submodule-based specification that can
    # reference the feat1 branch, so this configuration should be
    # suppressed.
    for each in generated_bldconfigs.cfg_build_configs:
        print(each)
        print('')
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "feat1" and
                    each.strategy == "submodules")


def test_example_internal_feat1_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="feat1",
                            strategy="HEADs",
                            description=BranchReq("R10", "feat1"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "feat1", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_develop_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="develop",
                            strategy="submodules",
                            description=MainBranch("R10", "develop"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "r3_master_head^9", "project_primary"),
                                  BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_develop_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="develop",
                            strategy="HEADs",
                            description=MainBranch("R10", "develop"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "primary", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

        # KWQ: this is also a no-exist because R10 doesn't have dev, so its master only dictates the submodules.  Note that if dev existed and had no submodules, then it should just be an R10 build.... TEST THIS
def test_example_internal_dev_regular_submodules(generated_bldconfigs):
    # Because dev is not a branch in R10, and all other repos are
    # submodules, there is no submodule-based specification that can
    # reference the dev branch, so this configuration should be
    # suppressed.
    for each in generated_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "dev" and
                    each.strategy == "submodules")



def test_example_internal_dev_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="dev",
                            strategy="HEADs",
                            description=BranchReq("R10", "dev"),
                            blds=[BldRepoRev("R10", "develop", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "primary", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_develop_submodules(example_hydra_jobsets):
    expected = dict([
        ( "develop.submodules-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R10, brr34:R3, brr34:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://gitlab.com/r10_url develop"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r3_url r3_master_head^9"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_explicit_default_url r4_master_head^1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=develop|strategy=submodules"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R10-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_develop_heads(example_hydra_jobsets):
    expected = dict([
          ("develop.HEADs-%s" % (G), {
             "checkinterval": 600,
             "description": "Build configuration: brr32:R10, brr32:R3, brr32:R4, ghcver=%s" % (G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R10-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "https://gitlab.com/r10_url develop"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "https://github.com/r3_url master"
                 },
                 "R4-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "https://github.com/r4_explicit_default_url primary"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=develop|strategy=HEADs"
                 },
             },
             "keepnr": 3,
             "nixexprinput": "R10-src",
             "nixexprpath": "./release.nix",
             "schedulingshares": 1
         }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_feat1_heads(example_hydra_jobsets):
    expected = dict([
        ( "feat1.HEADs-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R10, brr32:R3, brr32:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://gitlab.com/r10_url develop"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r3_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_explicit_default_url feat1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=feat1|strategy=HEADs"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R10-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_dev_heads(example_hydra_jobsets):
    expected = dict([
        ( "dev.HEADs-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R10, brr32:R3, brr32:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://gitlab.com/r10_url develop"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r3_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_explicit_default_url primary"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=dev|strategy=HEADs"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R10-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS  ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_bugfix9_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.submodules-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R10, brr34:R3, PR8192-brr31:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://gitlab.com/r10_url develop"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r3_url r3_master_head^9"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/remote_R4_y bugfix9"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=bugfix9|strategy=submodules|PR"
                 },
            },
            "keepnr": 3,
            "nixexprinput": "R10-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS  ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_bugfix9_heads(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.HEADs-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R10, brr32:R3, PR8192-brr31:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://gitlab.com/r10_url develop"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r3_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/remote_R4_y bugfix9"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=bugfix9|strategy=HEADs|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R10-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS  ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]
