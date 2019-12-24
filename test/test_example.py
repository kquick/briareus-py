from Briareus.Types import BldConfig, BldRepoRev, BldVariable
from git_example1 import GitExample1
import json
import pytest

input_spec = '''
{
  "Repos" : [ ("R1", "r1_url"),
              ("R2", "r2_url"),
              ("R3", "r3_url"),
              ("R5", "r5_url"),
              ("R6", "r6_url") ]
, "Branches" : [ "master", "feat1", "dev" ]
, "Variables" : {
      "ghcver" : [ "ghc844", "ghc865", "ghc881" ],
      "c_compiler" : [ "gnucc", "clang" ],
  }
, "Reporting" : {
      "logic": """
project_owner("R1", "george@_company.com").

project_owner("R3", "john@not_a_company.com").

action_type(email, "fred@nocompany.com", "R1").
action_type(email, "eddy@nocompany.com", "R1").
action_type(email, "sam@not_a_company.com", "R1").
action_type(email, "john@_company.com", "R1").
action_type(email, "anne@nocompany.com", "R1", master_submodules_broken).
      """
  }
}
'''

gitactor = GitExample1

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
    assert len(GS) * len(CS) * len(top_level) == len(set(generated_bldconfigs.cfg_build_configs))

def test_example_internal_blah_pullreq_submods(generated_bldconfigs):
    for each in [ BldConfig("R1", "pullreq", "blah", "submodules",
                            [
                                BldRepoRev("R1", "blah", "1", "X"),
                                BldRepoRev("R2", "r2_master_head^22", "project_primary"),
                                BldRepoRev("R3", "r3_master_head", "project_primary"),
                                BldRepoRev("R5", "blah", "project_primary"),
                                BldRepoRev("R6", "blah", "111"),
                                BldRepoRev("R7", "r7_master_head^4", "project_primary", "Y"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_blah_pullreq_HEADs(generated_bldconfigs):
    for each in [ BldConfig("R1", "pullreq", "blah", "HEADs",
                            [
                                BldRepoRev("R1", "blah", "1", "ignored"),
                                BldRepoRev("R2", "blah", "1111", 9999),
                                BldRepoRev("R3", "blah", "11"),
                                BldRepoRev("R5", "blah", "project_primary"),
                                BldRepoRev("R6", "blah", "111"),
                                BldRepoRev("R7", "master", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
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
    for each in [ BldConfig("R1", "pullreq", "bugfix9", "submodules",
                            [
                                BldRepoRev("R1", "master", "project_primary", "X"),
                                BldRepoRev("R2", "bugfix9", "23"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "bugfix9", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_bugfix9_pullreq_HEADs(generated_bldconfigs):
    for each in [ BldConfig("R1", "pullreq", "bugfix9", "HEADs",
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "bugfix9", "23"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "bugfix9", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "bugfix9", "8192"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
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
    for each in [ BldConfig("R1", "regular", "feat1", "submodules",
                            [
                                BldRepoRev("R1", "feat1", "project_primary"),
                                BldRepoRev("R2", "r2_master_head^1", "project_primary"),
                                BldRepoRev("R3", "r3_master_head", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "feat1", "project_primary"),
                                BldRepoRev("R4", "r4_feat1_head^2", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_feat1_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("R1", "regular", "feat1", "HEADs",
                            [
                                BldRepoRev("R1", "feat1", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "feat1", "project_primary"),
                                BldRepoRev("R4", "feat1", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig("R1", "regular", "master", "submodules",
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "r2_master_head", "project_primary"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("R1", "regular", "master", "HEADs",
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "master", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_submodules(generated_bldconfigs):
    for each in [ BldConfig("R1", "regular", "dev", "submodules",
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "r2_master_head", "project_primary"),
                                BldRepoRev("R3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("R5", "dev", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "r4_master_head^1", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        print(generated_bldconfigs)
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_HEADs(generated_bldconfigs):
    for each in [ BldConfig("R1", "regular", "dev", "HEADs",
                            [
                                BldRepoRev("R1", "master", "project_primary"),
                                BldRepoRev("R2", "master", "project_primary"),
                                BldRepoRev("R3", "master", "project_primary"),
                                BldRepoRev("R5", "dev", "project_primary"),
                                BldRepoRev("R6", "master", "project_primary"),
                                BldRepoRev("R4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("R1", "ghcver", G),
                                BldVariable("R1", "c_compiler", C),
                            ])
                  for G in GS for C in CS]:
        assert each in generated_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(CS) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_master_submodules(example_hydra_jobsets):
    expected = dict([
        ( "master.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:R1, brr4:R2, brr4:R3, brr4:R4, brr1:R5, brr1:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_heads(example_hydra_jobsets):
    expected = dict([
          ("master.HEADs-%s-%s" % (C,G), {
             "checkinterval": 600,
             "description": "Build configuration: brr1:R1, brr5:R2, brr5:R3, brr5:R4, brr1:R5, brr1:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
         }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_feat1_submodules(example_hydra_jobsets):
    expected = dict([
        ( "feat1.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:R1, brr4:R2, brr4:R3, brr4:R4, brr2:R5, brr1:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_feat1_heads(example_hydra_jobsets):
    expected = dict([
        ( "feat1.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:R1, brr6:R2, brr6:R3, brr5:R4, brr2:R5, brr1:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_dev_submodules(example_hydra_jobsets):
    expected = dict([
        ( "dev.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr2:R1, brr13:R2, brr13:R3, brr13:R4, brr1:R5, brr2:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_dev_heads(example_hydra_jobsets):
    expected = dict([
        ( "dev.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr2:R1, brr14:R2, brr14:R3, brr14:R4, brr1:R5, brr2:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_blah_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-blah.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: PR1-brr3:R1, brr9:R2, brr9:R3, brr1:R5, PR111-brr3:R6, brr9:R7, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
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
            "description": "Build configuration: PR1-brr3:R1, PR1111-brr3:R2, PR11-brr3:R3, brr1:R5, PR111-brr3:R6, brr7:R7, c_compiler=%s, ghcver=%s" % (C,G),
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
         }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_bugfix9_submodules(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.submodules-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr2:R1, PR23-brr3:R2, brr11:R3, PR8192-brr10:R4, brr1:R5, brr2:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
        }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_master_bugfix9_heads(example_hydra_jobsets):
    expected = dict([
        ( "PR-bugfix9.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: brr2:R1, PR23-brr3:R2, brr12:R3, PR8192-brr10:R4, brr1:R5, brr2:R6, c_compiler=%s, ghcver=%s" % (C,G),
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
         }) for G in GS for C in CS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]
