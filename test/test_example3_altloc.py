from Briareus.Types import BldConfig, BldRepoRev, BldVariable, BranchReq, PR_Grouped
from Briareus.VCS_API import PRSts_Active, PRInfo, SubModuleInfo
from Briareus.VCS.InternalMessages import PullReqInfo, RepoDesc
import test_example3 as tex3
from datetime import timedelta
import json
import pytest
import copy


# Like example3 except R3 and R10 are both explicitly listed with a
# RepoLoc translation in the input, and R10 adds a pull request. This
# test ensures that the Hydra input specifications use the correct
# location for primary and pull request references.

input_spec = open('test/inp_example3_altloc').read()

expected_repo_info = copy.deepcopy(tex3.expected_repo_info)
expected_repo_info['pullreqs'].add(
    PRInfo(pr_target_repo='R10',
           pr_srcrepo_url='git@r10_git_url:r10_url',
           pr_branch='devtest',
               pr_revision='r10_devtest_ref', pr_ident='321', pr_status=PRSts_Active(),
               pr_title='test R10 PR', pr_user='miles', pr_email='miles@to.go'))
expected_repo_info['submodules'].add(
    SubModuleInfo(sm_repo_name='R10', sm_branch='devtest', sm_pullreq_id='321', sm_sub_name='R3', sm_sub_vers='r3_master_head^7'))
expected_repo_info['submodules'].add(
    SubModuleInfo(sm_repo_name='R10', sm_branch='devtest', sm_pullreq_id='321', sm_sub_name='R4', sm_sub_vers='r4_master_head^11'))
expected_repo_info['subrepos'] = set(filter(lambda r: r.repo_name != 'R3', expected_repo_info['subrepos']))
expected_repo_info['subrepos'].add(
    RepoDesc(repo_name='R3',
             repo_url='git@r3_inp_url:r3_url',
             main_branch='master', project_repo=False))


build_output_time_budget = timedelta(seconds=1, milliseconds=250)  # avg 0.52s

@pytest.fixture(scope="module")
def example_hydra_jobsets(generated_hydra_builder_output):
    return generated_hydra_builder_output[0][None]

GS = [ "ghc822", "ghc844" ]
top_level = [
    "regular master heads",
    "regular master submodules",
    "regular feat1 heads",
    "regular dev heads",
    "pullreq bugfix9 heads",
    "pullreq bugfix9 submodules",
    "pullreq blah heads",
    "pullreq blah submodules",
    "pullreq devtest heads",
    "pullreq devtest submodules",
]

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
                            blds=[BldRepoRev("R10", "master", "project_primary"),
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
                            blds=[BldRepoRev("R10", "master", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_devtest_pullreq_submods(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="devtest",
                            strategy="submodules",
                            description=PR_Grouped("devtest"),
                            blds=[BldRepoRev("R10", "devtest", "321"),
                                  BldRepoRev("R3", "r3_master_head^7", "project_primary"),
                                  BldRepoRev("R4", "r4_master_head^11", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_devtest_pullreq_HEADs(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        print(each)
        print('')
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="devtest",
                            strategy="HEADs",
                            description=PR_Grouped("devtest"),
                            blds=[BldRepoRev("R10", "devtest", "321"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "master", "project_primary"),
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
        assert not (each.branchtype == "regular" and
                    each.branchname == "feat1" and
                    each.strategy == "submodules")


def test_example_internal_feat1_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="feat1",
                            strategy="HEADs",
                            description=BranchReq("R10", "feat1"),
                            blds=[BldRepoRev("R10", "master", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "feat1", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="master",
                            strategy="submodules",
                            description=BranchReq("R10", "master"),
                            blds=[BldRepoRev("R10", "master", "project_primary"),
                                  BldRepoRev("R3", "r3_master_head^9", "project_primary"),
                                  BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="master",
                            strategy="HEADs",
                            description=BranchReq("R10", "master"),
                            blds=[BldRepoRev("R10", "master", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "master", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs

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
                            blds=[BldRepoRev("R10", "master", "project_primary"),
                                  BldRepoRev("R3", "master", "project_primary"),
                                  BldRepoRev("R4", "master", "project_primary"),
                            ],
                            bldvars=[BldVariable("R10", "ghcver", G)])
                  for G in GS]:
        assert each in generated_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_master_submodules(example_hydra_jobsets):
    expected = dict([
        ( "master.submodules-%s" % (G), {
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
                    "value": "git@r10_git_url:r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r3_inp_url:r3_url r3_master_head^9"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_url r4_master_head^1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=master|strategy=submodules"
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

def test_example_hydra_master_heads(example_hydra_jobsets):
    expected = dict([
          ("master.HEADs-%s" % (G), {
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
                     "value": "git@r10_git_url:r10_url master"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "git@r3_inp_url:r3_url master"
                 },
                 "R4-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "https://github.com/r4_url master"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=master|strategy=HEADs"
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
                    "value": "git@r10_git_url:r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r3_inp_url:r3_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_url feat1"
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
                    "value": "git@r10_git_url:r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r3_inp_url:r3_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_url master"
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

def test_example_hydra_master_bugfix9_submodules(example_hydra_jobsets):
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
                    "value": "git@r10_git_url:r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r3_inp_url:r3_url r3_master_head^9"
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

def test_example_hydra_master_bugfix9_heads(example_hydra_jobsets):
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
                     "value": "git@r10_git_url:r10_url master"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "git@r3_inp_url:r3_url master"
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

######################################################################

def test_example_hydra_master_devtest_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-devtest.submodules-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: PR321-brr31:R10, brr34:R3, brr34:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r10_git_url:r10_url devtest"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "git@r3_inp_url:r3_url r3_master_head^7"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "https://github.com/r4_url r4_master_head^11"  # KWQ?: r4_url or other?
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=devtest|strategy=submodules|PR"
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

def test_example_hydra_master_devtest_heads(example_hydra_jobsets):
    expected = dict([
        ( "PR-devtest.HEADs-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: PR321-brr31:R10, brr32:R3, brr32:R4, ghcver=%s" % (G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R10-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "git@r10_git_url:r10_url devtest"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "git@r3_inp_url:r3_url master"
                 },
                 "R4-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "https://github.com/r4_url master"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=devtest|strategy=HEADs|PR"
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
