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
  "Repos" : [ ("Repo1", "r1_url"),
              ("Repo2", "r2_url"),
              ("Repo3", "r3_url"),
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
project("Repo1").
repo("Repo1").
repo("Repo2").
repo("Repo3").
subrepo("Repo4").
branchreq("Repo1", "master").
branchreq("Repo1", "develop").
branch("Repo3", "develop").
branch("Repo4", "master").
branch("Repo1", "develop").
branch("Repo2", "master").
branch("Repo3", "master").
branch("Repo2", "develop").
branch("Repo1", "master").
submodule("Repo1", "develop", "Repo2", "r2_develop_head").
submodule("Repo1", "develop", "Repo3", "r3_develop_head").
submodule("Repo1", "develop", "Repo4", "r4_master_head").
submodule("Repo1", "master", "Repo2", "r2_master_head").
submodule("Repo1", "master", "Repo3", "r3_master_head^3").
submodule("Repo1", "master", "Repo4", "r4_master_head^1").
varname("Repo1", "ghcver").
var("Repo1", "ghcver", "ghc844").
var("Repo1", "ghcver", "ghc865").
var("Repo1", "ghcver", "ghc881").
'''.split('\n')))


def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

def test_example_internal_regular_develop_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "submodules",
                                [BldRepoRev("Repo1","develop"),
                                 BldRepoRev("Repo2","r2_develop_head"),
                                 BldRepoRev("Repo3","r3_develop_head"),
                                 BldRepoRev("Repo4","r4_master_head")],
                                [BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "HEADs",
                                [BldRepoRev("Repo1","develop"),
                                 BldRepoRev("Repo2","develop"),
                                 BldRepoRev("Repo3","develop"),
                                 BldRepoRev("Repo4","master")],
                                [BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "submodules",
                                [BldRepoRev("Repo1","master"),
                                 BldRepoRev("Repo2","r2_master_head"),
                                 BldRepoRev("Repo3","r3_master_head^3"),
                                 BldRepoRev("Repo4","r4_master_head^1")],
                                [BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "HEADs",
                                [BldRepoRev("Repo1","master"),
                                 BldRepoRev("Repo2","master"),
                                 BldRepoRev("Repo3","master"),
                                 BldRepoRev("Repo4","master")],
                                [BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs



def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))
