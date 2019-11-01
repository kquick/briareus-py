import Briareus.BCGen.Operations as BCGen
from Briareus.Types import BldConfig, BldRepoRev, BldVariable
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
from git_example1 import GitExample1
import json
import pytest

# Different top-level R10 shares R3 and R4 with test_example.py.  This also has different variables.

input_spec = '''
{
  "Repos" : [ ("R10", "r10_url"),
            ]
, "Branches" : [ "master", "feat1", "dev" ]
, "Variables" : {
      "ghcver" : [ "ghc822", "ghc844" ],
  }
}
'''


@pytest.fixture(scope="module")
def example_internal_bldconfigs():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample1, globalName="GetGitInfo")
        inp, repo_info = BInput.input_desc_and_VCS_info(input_spec, verbose=True,
                                                        actor_system=asys)
        gen = Generator.Generator(actor_system=asys, verbose=True)
        (_rtype, cfgs) = gen.generate_build_configs(inp, repo_info)
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
        input_desc, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                               verbose=True,
                                                               actor_system=asys)
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        output = bcgen.generate(input_desc, repo_info)
        yield output[0]
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

GS = [ "ghc822", "ghc844" ]
top_level = [
    "regular master heads",
    "regular master submodules",
    "regular feat1 heads",
    "regular dev heads",
    "pullreq bugfix9 heads",
    "pullreq bugfix9 submodules",
]

def test_example_internal_count(example_internal_bldconfigs):
    print('### bldcfgs:')
    for each in example_internal_bldconfigs.cfg_build_configs:
        print(each.projectname, each.branchtype, each.branchname, each.strategy)
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

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
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="bugfix9",
                            strategy="submodules",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","r3_master_head^9"),
                                  BldRepoRev("R4","bugfix9"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_bugfix9_pullreq_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="pullreq",
                            branchname="bugfix9",
                            strategy="HEADs",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","master"),
                                  BldRepoRev("R4","bugfix9"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
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

def test_example_internal_no_feat1_regular_submodules(example_internal_bldconfigs):
    # Because feat1 is not a branch in R10, and all other repos are
    # submodules, there is no submodule-based specification that can
    # reference the feat1 branch, so this configuration should be
    # suppressed.
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "feat1" and
                    each.strategy == "submodules")


def test_example_internal_feat1_regular_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="feat1",
                            strategy="HEADs",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","master"),
                                  BldRepoRev("R4","feat1"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_submodules(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="master",
                            strategy="submodules",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","r3_master_head^9"),
                                  BldRepoRev("R4","r4_master_head^1"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_master_regular_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="master",
                            strategy="HEADs",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","master"),
                                  BldRepoRev("R4","master"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_dev_regular_submodules(example_internal_bldconfigs):
    # Because dev is not a branch in R10, and all other repos are
    # submodules, there is no submodule-based specification that can
    # reference the dev branch, so this configuration should be
    # suppressed.
    for each in example_internal_bldconfigs.cfg_build_configs:
        assert not (each.branchtype == "regular" and
                    each.branchname == "dev" and
                    each.strategy == "submodules")



def test_example_internal_dev_regular_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="R10",
                            branchtype="regular",
                            branchname="dev",
                            strategy="HEADs",
                            blds=[BldRepoRev("R10","master"),
                                  BldRepoRev("R3","master"),
                                  BldRepoRev("R4","master"),
                            ],
                            bldvars=[BldVariable("R10","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_master_submodules(example_hydra_jobsets):
    expected = dict([
        ( "master.submodules-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:R10, brr4:R3, brr4:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head^9"
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
             "description": "Build configuration: brr1:R10, brr5:R3, brr5:R4, ghcver=%s" % (G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R10-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r10_url master"
                 },
                 "R3-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r3_url master"
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
            "description": "Build configuration: brr2:R10, brr14:R3, brr15:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
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
            "description": "Build configuration: brr2:R10, brr14:R3, brr14:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
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
            "description": "Build configuration: brr2:R10, brr11:R3, PR8192-brr10:R4, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "R10-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r10_url master"
                },
                "R3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url r3_master_head^9"
                },
                "R4-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_R4_y bugfix9"
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
            "description": "Build configuration: brr2:R10, brr12:R3, PR8192-brr10:R4, ghcver=%s" % (G),
             "emailoverride": "",
             "enabled": 1,
             "enableemail": False,
             "hidden": False,
             "inputs": {
                 "R10-src": {
                     "emailresponsible": False,
                     "type": "git",
                     "value": "r10_url master"
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
