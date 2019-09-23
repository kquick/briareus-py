import Briareus.BCGen.Operations as BCGen
import Briareus.BCGen.BuildConfigs as CFG
import Briareus.BCGen.Parser as Parser
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
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample2, globalName="GetGitInfo")
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
        parser = Parser.BISParser()
        gen = Generator.Generator(actor_system = asys)
        inp = parser.parse(input_spec)
        (rtype, facts) = gen.generate_build_configs(inp, up_to="facts")
        assert rtype == "facts"
        assert expected_facts == sorted(map(str, facts))
    finally:
        asys.shutdown()


expected_facts = sorted(filter(None, '''
:- dynamic project/1.
:- dynamic repo/1.
:- dynamic subrepo/1.
:- dynamic submodule/4.
:- dynamic branch/1.
:- dynamic branch/2.
:- dynamic pullreq/3.
:- dynamic varname/1.
:- dynamic var/2.
project("R1").
repo("R1").
repo("R2").
repo("R3").
subrepo("R4").
branch("master").
branch("develop").
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
varname("ghcver").
var("ghcver", "ghc844").
var("ghcver", "ghc865").
var("ghcver", "ghc881").
'''.split('\n')))


def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

def test_example_internal_regular_develop_submodules(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "develop", "submodules",
                                [CFG.BldRepoRev("R1","develop"),
                                 CFG.BldRepoRev("R2","r2_develop_head"),
                                 CFG.BldRepoRev("R3","r3_develop_head"),
                                 CFG.BldRepoRev("R4","r4_master_head")],
                                [CFG.BldVariable("ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "develop", "HEADs",
                                [CFG.BldRepoRev("R1","develop"),
                                 CFG.BldRepoRev("R2","develop"),
                                 CFG.BldRepoRev("R3","develop"),
                                 CFG.BldRepoRev("R4","master")],
                                [CFG.BldVariable("ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_submodules(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "master", "submodules",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","r2_master_head"),
                                 CFG.BldRepoRev("R3","r3_master_head^3"),
                                 CFG.BldRepoRev("R4","r4_master_head^1")],
                                [CFG.BldVariable("ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_HEADs(example_internal_bldconfigs):
    for each in [ CFG.BldConfig("regular", "master", "HEADs",
                                [CFG.BldRepoRev("R1","master"),
                                 CFG.BldRepoRev("R2","master"),
                                 CFG.BldRepoRev("R3","master"),
                                 CFG.BldRepoRev("R4","master")],
                                [CFG.BldVariable("ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs



def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))
