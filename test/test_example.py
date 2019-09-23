import Briareus.BCGen.Operations as BCGen
import Briareus.BCGen.BuildConfigs as CFG
import Briareus.Input.Parser as Parser
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
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
}
'''


@pytest.fixture(scope="module")
def example_internal_bldconfigs():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample1, globalName="GetGitInfo")
        parser = Parser.BISParser(verbose=True)
        gen = Generator.Generator(actor_system=asys, verbose=True)
        (_rtype, cfgs) = gen.generate_build_configs(parser.parse(input_spec))
        yield cfgs
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()


@pytest.fixture(scope="module")
def example_hydra_jobsets():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample1, globalName="GetGitInfo")
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        output = bcgen.generate(input_spec)
        yield output[0]
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

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

def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(CS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

def test_example_internal_blah_pullreq_submods(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("pullreq", "blah", "submodules",
                                [CFG.BldRepoRev("R1","blah", "X"),
                                 CFG.BldRepoRev("R2","r2_master_head"),
                                 CFG.BldRepoRev("R3","r3_master_head"),
                                 CFG.BldRepoRev("R5","blah"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R7","r7_master_head^4", "Y")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_blah_pullreq_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("pullreq", "blah", "HEADs",
                                [CFG.BldRepoRev("R1","blah", "ignored"),
                                 CFG.BldRepoRev("R2","master", 9999),
                                 CFG.BldRepoRev("R3","blah"),
                                 CFG.BldRepoRev("R5","blah"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R7","master")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_no_blah_regular_submods(example_internal_bldconfigs):
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "blah" and
                    each.strategy == "submodules")

def test_example_internal_no_blah_regular_HEADs(example_internal_bldconfigs):
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "blah" and
                    each.strategy == "HEADs")

def test_example_internal_bugfix9_pullreq_submods(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("pullreq", "bugfix9", "submodules",
                                [CFG.BldRepoRev("R1","master", "X"),
                                 CFG.BldRepoRev("R2","bugfix9"),
                                 CFG.BldRepoRev("R3","r3_master_head^3"),
                                 CFG.BldRepoRev("R5","bugfix9"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","bugfix9")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_bugfix9_pullreq_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("pullreq", "bugfix9", "HEADs",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","bugfix9"),
                                 CFG.BldRepoRev("R3","master"),
                                 CFG.BldRepoRev("R5","bugfix9"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","bugfix9")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_no_bugfix9_regular_submods(example_internal_bldconfigs):
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "bugfix9" and
                    each.strategy == "submodules")

def test_example_internal_no_bugfix9_regular_HEADs(example_internal_bldconfigs):
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "bugfix9" and
                    each.strategy == "HEADs")

def test_example_internal_feat1_regular_submodules(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "feat1", "submodules",
                                [CFG.BldRepoRev("R1","feat1"),
                                 CFG.BldRepoRev("R2","r2_master_head^1"),
                                 CFG.BldRepoRev("R3","r3_master_head"),
                                 CFG.BldRepoRev("R5","master"),
                                 CFG.BldRepoRev("R6","feat1"),
                                 CFG.BldRepoRev("R4","r4_feat1_head^2")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_feat1_regular_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "feat1", "HEADs",
                                [CFG.BldRepoRev("R1","feat1"),
                                 CFG.BldRepoRev("R2","master"),
                                 CFG.BldRepoRev("R3","master"),
                                 CFG.BldRepoRev("R5","master"),
                                 CFG.BldRepoRev("R6","feat1"),
                                 CFG.BldRepoRev("R4","feat1")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_submodules(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "master", "submodules",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","r2_master_head"),
                                 CFG.BldRepoRev("R3","r3_master_head^3"),
                                 CFG.BldRepoRev("R5","master"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","r4_master_head^1")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "master", "HEADs",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","master"),
                                 CFG.BldRepoRev("R3","master"),
                                 CFG.BldRepoRev("R5","master"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","master")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_submodules(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "dev", "submodules",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","r2_master_head"),
                                 CFG.BldRepoRev("R3","r3_master_head^3"),
                                 CFG.BldRepoRev("R5","dev"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","r4_master_head^1")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        print(example_internal_bldconfigs)
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "dev", "HEADs",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","master"),
                                 CFG.BldRepoRev("R3","master"),
                                 CFG.BldRepoRev("R5","dev"),
                                 CFG.BldRepoRev("R6","master"),
                                 CFG.BldRepoRev("R4","master")],
                                [CFG.BldVariable("ghcver", G), CFG.BldVariable("c_compiler", C)])
                  for G in GS for C in CS]:
        assert each in example_internal_bldconfigs.cfg_build_configs


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
            "description": "Build configuration: PR1-brr3:R1, brr9:R2, brr9:R3, brr1:R5, brr2:R6, brr9:R7, c_compiler=%s, ghcver=%s" % (C,G),
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
                    "value": "r2_url r2_master_head"
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
                    "value": "r6_url master"
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


def test_example_hydra_master_blah_heads(example_hydra_jobsets):
    expected = dict([
        ( "PR-blah.HEADs-%s-%s" % (C,G), {
            "checkinterval": 600,
            "description": "Build configuration: PR1-brr3:R1, brr7:R2, brr8:R3, brr1:R5, brr2:R6, brr7:R7, c_compiler=%s, ghcver=%s" % (C,G),
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
                     "value": "r2_url master"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r3_url blah"
                 },
                 "R5-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r5_url blah"
                 },
                 "R6-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r6_url master"
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
