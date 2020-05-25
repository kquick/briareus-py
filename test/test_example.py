from Briareus.Types import BldConfig, BldRepoRev, BldVariable, BranchReq, PR_Grouped
from Briareus.VCS.InternalMessages import (BranchRef, PRInfo,
                                           PRSts_Active, PRSts_Merged, PRSts_Closed, PRSts_New,
                                           RepoDesc, SubModuleInfo)
from git_example1 import GitExample1
import json
import pytest

gitactor = GitExample1
input_spec = open('test/inp_example').read()

expected_repo_info = {
    'branches' : set([
        BranchRef(reponame='R1', branchname='feat1', branchref='r1-feat1-ref'),
        BranchRef(reponame='R1', branchname='master', branchref='R1-master-ref'),
        BranchRef(reponame='R2', branchname='bugfix9', branchref='r2-bugfix9-ref'),
        BranchRef(reponame='R2', branchname='master', branchref='R2-master-ref'),
        BranchRef(reponame='R3', branchname='blah', branchref='r3-blah-ref'),
        BranchRef(reponame='R3', branchname='master', branchref='R3-master-ref'),
        BranchRef(reponame='R4', branchname='feat1', branchref='r4-feat1-ref'),
        BranchRef(reponame='R4', branchname='master', branchref='R4-master-ref'),
        BranchRef(reponame='R5', branchname='blah', branchref='r5-blah-ref'),
        BranchRef(reponame='R5', branchname='bugfix9', branchref='r5-bugfix9-ref'),
        BranchRef(reponame='R5', branchname='dev', branchref='r5-dev-ref'),
        BranchRef(reponame='R5', branchname='master', branchref='R5-master-ref'),
        BranchRef(reponame='R6', branchname='feat1', branchref='r6-feat1-ref'),
        BranchRef(reponame='R6', branchname='master', branchref='R6-master-ref'),
        BranchRef(reponame='R7', branchname='master', branchref='R7-master-ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='R1', pr_srcrepo_url='remote_R1_b', pr_branch='blah',
               pr_revision='r1_blah_mergeref', pr_ident='1', pr_status=PRSts_Active(),
               pr_title='pr#19', pr_user='nick', pr_email='nick@bad.seeds'),
        PRInfo(pr_target_repo='R2', pr_srcrepo_url='remote_r2_a', pr_branch='bugfix9',
               pr_revision='r2_b9_mergeref', pr_ident='23', pr_status=PRSts_Active(),
               pr_title='add fantasticness', pr_user='banana', pr_email=''),
        PRInfo(pr_target_repo='R2', pr_srcrepo_url='remote_r2_pr1111_url', pr_branch='blah',
               pr_revision='r2_blah_mergeref', pr_ident='1111', pr_status=PRSts_Active(),
               pr_title='blah also', pr_user='not_nick', pr_email='not_nick@bad.seeds'),
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='remote_r3_CLOSED_url', pr_branch='closed_pr',
               pr_revision='r3_CLOSED_mergeref', pr_ident='22', pr_status=PRSts_Closed(),
               pr_title='ignored because closed', pr_user='done', pr_email='done@already.yo'),
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='remote_r3_MERGED_url', pr_branch='merged_pr',
               pr_revision='r3_MERGED_mergeref', pr_ident='33', pr_status=PRSts_Merged(),
               pr_title='ignored because merged', pr_user='done', pr_email='done@already.yo'),
        PRInfo(pr_target_repo='R3', pr_srcrepo_url='remote_r3_pr11_url', pr_branch='blah',
               pr_revision='r3_blah_mergeref', pr_ident='11', pr_status=PRSts_Active(),
               pr_title='blah started', pr_user='nick', pr_email='nick@bad.seeds'),
        PRInfo(pr_target_repo='R4', pr_srcrepo_url='remote_R4_y', pr_branch='bugfix9',
               pr_revision='r1_bf9_mergeref', pr_ident='8192', pr_status=PRSts_New(),
               pr_title='fix ninth bug!', pr_user='ozzie', pr_email='ozzie@crazy.train'),
        PRInfo(pr_target_repo='R6', pr_srcrepo_url='remote_r6_pr111_url', pr_branch='blah',
               pr_revision='r6_blah_mergeref', pr_ident='111', pr_status=PRSts_Active(),
               pr_title='blah match', pr_user='nick', pr_email='nick@bad.seeds'),
    ]),
    'submodules': set([
        SubModuleInfo(sm_repo_name='R1', sm_branch='blah', sm_pullreq_id='1', sm_sub_name='R2', sm_sub_vers='r2_master_head^22'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='blah', sm_pullreq_id='1', sm_sub_name='R3', sm_sub_vers='r3_master_head'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='blah', sm_pullreq_id='1', sm_sub_name='R7', sm_sub_vers='r7_master_head^4'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='feat1', sm_pullreq_id=None, sm_sub_name='R2', sm_sub_vers='r2_master_head^1'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='feat1', sm_pullreq_id=None, sm_sub_name='R3', sm_sub_vers='r3_master_head'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='feat1', sm_pullreq_id=None, sm_sub_name='R4', sm_sub_vers='r4_feat1_head^2'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='master', sm_pullreq_id=None, sm_sub_name='R2', sm_sub_vers='r2_master_head'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='master', sm_pullreq_id=None, sm_sub_name='R3', sm_sub_vers='r3_master_head^3'),
        SubModuleInfo(sm_repo_name='R1', sm_branch='master', sm_pullreq_id=None, sm_sub_name='R4', sm_sub_vers='r4_master_head^1'),
    ]),
    'subrepos': set([
        RepoDesc(repo_name='R2', repo_url='r2_url', main_branch='master', project_repo=False),
        RepoDesc(repo_name='R3', repo_url='r3_url', main_branch='master', project_repo=False),
        RepoDesc(repo_name='R4', repo_url='r4_url', main_branch='master', project_repo=False),
        RepoDesc(repo_name='R7', repo_url='r7_url', main_branch='master', project_repo=False),
    ]),
}


@pytest.fixture(scope="module")
def example_hydra_jobsets(generated_hydra_builder_output):
    return generated_hydra_builder_output[0][None]

GS = [ "ghc844", "ghc865", "ghc881" ]
CS = [ "gnucc", "clang" ]
top_level = [
    "regular master heads",
    "regular master submodules",
    "regular feat1 heads",
    "regular feat1 submodules",
    "regular dev heads",
    "regular dev submodules",
    "pullreq blah submodules",
    "pullreq blah heads",
    "pullreq bugfix9 heads",
    "pullreq bugfix9 submodules",
]

def test_example_internal_count(generated_bldconfigs):
    assert (len(GS) + len(CS) - 1) * len(top_level) == len(generated_bldconfigs.cfg_build_configs)

def test_example_internal_blah_pullreq_submods(generated_bldconfigs):
    # Note that R1 has a pullreq for the blah branch, and this pullreq
    # has submodule specifications that are respected for the
    # "submodules" strategy, therefore the blah PR's for R2 and R3 are
    # ignored.  They will be used for the "heads" strategy though.
    for each in [ BldConfig("Project #1", "pullreq", "blah", "submodules",
                            PR_Grouped("blah"),
                            [
                                BldRepoRev("R1", "blah", "1", "X"),
                                BldRepoRev("R2", "r2_master_head^22", "project_primary"),
                                BldRepoRev("R3", "r3_master_head", "project_primary"),
                                BldRepoRev("R5", "blah", "project_primary"),
                                BldRepoRev("R6", "blah", "111"),
                                BldRepoRev("R7", "r7_master_head^4", "project_primary", "Y"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_blah_pullreq_HEADs(generated_bldconfigs):
    # See note for the submodules strategy test.
    for each in [ BldConfig("Project #1", "pullreq", "blah", "HEADs",
                            PR_Grouped("blah"),
                            [
                                BldRepoRev("R1", "blah", "1", "ignored"),
                                BldRepoRev("R2", "blah", "1111", 9999),
                                BldRepoRev("R3", "blah", "11"),
                                BldRepoRev("R5", "blah", "project_primary"),
                                BldRepoRev("R6", "blah", "111"),
                                BldRepoRev("R7", "master", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

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
    for each in [ BldConfig("Project #1", "pullreq", "bugfix9", "submodules",
                            PR_Grouped("bugfix9"),
                            [
                                BldRepoRev("R1", "master", "project_primary", "X"),
                                BldRepoRev("R2", "bugfix9", "23"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "bugfix9", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_bugfix9_pullreq_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "pullreq", "bugfix9", "HEADs",
                            PR_Grouped("bugfix9"),
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "bugfix9", "23"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "bugfix9", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
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

def test_example_internal_feat1_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "feat1", "submodules",
                            BranchReq("Project #1", "feat1"),
                            [
                                BldRepoRev("R1", "feat1", "project_primary"),
                                BldRepoRev("R2", "r2_master_head^1", "project_primary"),
                                BldRepoRev("R3", "r3_master_head", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "feat1", "project_primary"),
                                BldRepoRev("R4", "r4_feat1_head^2", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_feat1_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "feat1", "HEADs",
                            BranchReq("Project #1", "feat1"),
                            [
                                BldRepoRev("R1", "feat1", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "feat1", "project_primary"),
                                BldRepoRev("R4", "feat1", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "master", "submodules",
                            BranchReq("Project #1", "master"),
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "r2_master_head", "project_primary"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "master", "HEADs",
                            BranchReq("Project #1", "master"),
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "dev", "submodules",
                            BranchReq("Project #1", "dev"),
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "r2_master_head", "project_primary"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "dev", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        print(generated_bldconfigs)
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Project #1", "regular", "dev", "HEADs",
                            BranchReq("Project #1", "dev"),
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "dev", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("Project #1", "ghcver", G),
                                BldVariable("Project #1", "c_compiler", C),
                            ])
                  for G in GS
                  for C in (CS if G == GS[0] else CS[:1])]:
        assert each in generated_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert (len(GS) + len(CS) - 1) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_master_submodules(example_hydra_jobsets):
    expected = dict([
        ( "master.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R1, brr34:R2, brr34:R3, brr34:R4, brr34:R5, brr34:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url r2_master_head"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head^3"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url master"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r4_url r4_master_head^1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=master|strategy=submodules"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_heads(example_hydra_jobsets):
    expected = dict([
          ("master.HEADs-%s-%s" % (C,G), {
             "checkinterval": 600,
             "description": "Build configuration: brr32:R1, brr32:R2, brr32:R3, brr32:R4, brr32:R5, brr32:R6, c_compiler=%s, ghcver=%s" % (C,G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R1-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r1_url master"
                 },
                 "R2-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r2_url master"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r3_url master"
                 },
                 "R5-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r5_url master"
                 },
                 "R6-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r6_url master"
                 },
                 "R4-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r4_url master"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "c_compiler": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": C
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=master|strategy=HEADs"
                 },
             },
             "keepnr": 3,
             "nixexprinput": "R1-src",
             "nixexprpath": "./release.nix",
             "schedulingshares": 1
         })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_feat1_submodules(example_hydra_jobsets):
    expected = dict([
        ( "feat1.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R1, brr34:R2, brr34:R3, brr34:R4, brr34:R5, brr34:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url feat1"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url r2_master_head^1"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url master"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url feat1"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r4_url r4_feat1_head^2"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=feat1|strategy=submodules"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_feat1_heads(example_hydra_jobsets):
    expected = dict([
        ( "feat1.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R1, brr32:R2, brr32:R3, brr32:R4, brr32:R5, brr32:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url feat1"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url master"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url feat1"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r4_url feat1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=feat1|strategy=HEADs"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_dev_submodules(example_hydra_jobsets):
    expected = dict([
        ( "dev.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R1, brr34:R2, brr34:R3, brr34:R4, brr34:R5, brr34:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url r2_master_head"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head^3"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url dev"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r4_url r4_master_head^1"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=dev|strategy=submodules"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_dev_heads(example_hydra_jobsets):
    expected = dict([
        ( "dev.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R1, brr32:R2, brr32:R3, brr32:R4, brr32:R5, brr32:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url dev"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url master"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r4_url master"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=dev|strategy=HEADs"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_blah_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-blah.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: PR1-brr31:R1, brr34:R2, brr34:R3, brr30:R5, PR111-brr31:R6, brr34:R7, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_R1_b blah"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url r2_master_head^22"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url blah"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_r6_pr111_url blah"
                },
                "R7-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r7_url r7_master_head^4"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=blah|strategy=submodules|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]


def test_example_hydra_blah_heads(example_hydra_jobsets):
    # Note that the test_example_hydra_blah_submodules ignores the
    # PR11 in R3 because the submodules spec in PR1 of R1 takes
    # priority.  In *this* test, the submodules are ignored, so PR11
    # now expresses.
    expected = dict([
        ( "PR-blah.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: PR1-brr31:R1, PR1111-brr31:R2, PR11-brr31:R3, brr30:R5, PR111-brr31:R6, brr32:R7, c_compiler=%s, ghcver=%s" % (C,G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R1-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_R1_b blah"
                 },
                 "R2-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_r2_pr1111_url blah"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_r3_pr11_url blah"
                 },
                 "R5-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r5_url blah"
                 },
                 "R6-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_r6_pr111_url blah"
                 },
                 "R7-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r7_url master"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "c_compiler": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": C
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=blah|strategy=HEADs|PR"
                 },
             },
             "keepnr": 3,
             "nixexprinput": "R1-src",
             "nixexprpath": "./release.nix",
             "schedulingshares": 1
         })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_bugfix9_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr34:R1, PR23-brr31:R2, brr34:R3, PR8192-brr31:R4, brr30:R5, brr34:R6, c_compiler=%s, ghcver=%s" % (C,G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "R2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_r2_a bugfix9"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head^3"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_R4_y bugfix9"
                },
                "R5-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r5_url bugfix9"
                },
                "R6-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r6_url master"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "c_compiler": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": C
                },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=bugfix9|strategy=submodules|PR"
                 },
            },
            "keepnr": 3,
            "nixexprinput": "R1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_bugfix9_heads(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr32:R1, PR23-brr31:R2, brr32:R3, PR8192-brr31:R4, brr30:R5, brr32:R6, c_compiler=%s, ghcver=%s" % (C,G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R1-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r1_url master"
                 },
                 "R2-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_r2_a bugfix9"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r3_url master"
                 },
                 "R4-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "remote_R4_y bugfix9"
                 },
                 "R5-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r5_url bugfix9"
                 },
                 "R6-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r6_url master"
                 },
                 "ghcver": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": G
                 },
                 "c_compiler": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": C
                 },
                 "variant": {
                     "emailresponsible": False,
                     "type": "string",
                     "value": "|branch=bugfix9|strategy=HEADs|PR"
                 },
             },
             "keepnr": 3,
             "nixexprinput": "R1-src",
             "nixexprpath": "./release.nix",
             "schedulingshares": 1
         })
        for G in GS
        for C in (CS if G == GS[0] else CS[:1])])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]
