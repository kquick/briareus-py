import Briareus.BCGen.Operations as BCGen
from Briareus.Types import BldConfig, BldRepoRev, BldVariable
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
from git_example2 import GitExample2
import json
import pytest

input_spec = '''
{
  "Repos" : [ ("R1", "r1_url"),
              ("R2", "r2_url"),
              ("R3", "r3_url"),
            ]
, "Branches" : [ "master", "develop" ]
, "Variables" : {
      "ghcver" : [ "ghc844", "ghc865", "ghc881" ],
  }
}
'''


@pytest.fixture(scope="module")
def example_internal_bldconfigs():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample2, globalName="GetGitInfo")
        inp, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                        actor_system=asys,
                                                        verbose=True)
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
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample2, globalName="GetGitInfo")
        inp_desc, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                             actor_system=asys,
                                                             verbose=True)
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        output = bcgen.generate(inp_desc, repo_info)
        yield output[0]
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

GS = [ "ghc844", "ghc865", "ghc881" ]
top_level = [
    "regular develop HEADs",
    "regular develop submodules",
    "regular master HEADs",
    "regular master submodules",
]

def test_example_facts():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample2, globalName="GetGitInfo")
        # Replication of BCGen.Operations.BCGengenerate()
        inp, repo_info = BInput.input_desc_and_VCS_info(input_spec, verbose=True,
                                                        actor_system=asys)
        gen = Generator.Generator(actor_system = asys)
        (rtype, facts) = gen.generate_build_configs(inp, repo_info, up_to="facts")
        assert rtype == "facts"
        assert expected_facts == sorted(map(str, facts))
    finally:
        asys.shutdown()


expected_facts = sorted(filter(None, '''
:- dynamic project/1.
:- dynamic repo/1.
:- dynamic subrepo/1.
:- dynamic submodule/4.
:- dynamic branchreq/2.
:- dynamic branch/2.
:- dynamic pullreq/3.
:- dynamic varname/2.
:- dynamic var/3.
project("R1").
repo("R1").
repo("R2").
repo("R3").
subrepo("R4").
branchreq("R1", "master").
branchreq("R1", "develop").
branch("R3", "develop").
branch("R4", "master").
branch("R1", "develop").
branch("R2", "master").
branch("R3", "master").
branch("R2", "develop").
branch("R1", "master").
submodule("R1", "develop", "R2", "r2_develop_head").
submodule("R1", "develop", "R3", "r3_develop_head").
submodule("R1", "develop", "R4", "r4_master_head").
submodule("R1", "master", "R2", "r2_master_head").
submodule("R1", "master", "R3", "r3_master_head^3").
submodule("R1", "master", "R4", "r4_master_head^1").
varname("R1", "ghcver").
var("R1", "ghcver", "ghc844").
var("R1", "ghcver", "ghc865").
var("R1", "ghcver", "ghc881").
'''.split('\n')))


def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

def test_example_internal_regular_develop_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("regular", "develop", "submodules",
                                [BldRepoRev("R1","develop"),
                                 BldRepoRev("R2","r2_develop_head"),
                                 BldRepoRev("R3","r3_develop_head"),
                                 BldRepoRev("R4","r4_master_head")],
                                [BldVariable("R1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("regular", "develop", "HEADs",
                                [BldRepoRev("R1","develop"),
                                 BldRepoRev("R2","develop"),
                                 BldRepoRev("R3","develop"),
                                 BldRepoRev("R4","master")],
                                [BldVariable("R1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("regular", "master", "submodules",
                                [BldRepoRev("R1","master"),
                                 BldRepoRev("R2","r2_master_head"),
                                 BldRepoRev("R3","r3_master_head^3"),
                                 BldRepoRev("R4","r4_master_head^1")],
                                [BldVariable("R1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("regular", "master", "HEADs",
                                [BldRepoRev("R1","master"),
                                 BldRepoRev("R2","master"),
                                 BldRepoRev("R3","master"),
                                 BldRepoRev("R4","master")],
                                [BldVariable("R1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs



def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))
