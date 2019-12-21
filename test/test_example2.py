import Briareus.AnaRep.Operations as AnaRep
import Briareus.BCGen.Operations as BCGen
from Briareus.Types import (BldConfig, BldRepoRev, BldVariable,
                            ProjectSummary, StatusReport, VarFailure)
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
from git_example2 import GitExample2
import json
import pytest
from datetime import datetime, timedelta

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
def example_hydra_builder_output():
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

@pytest.fixture(scope="module")
def example_hydra_jobsets(example_hydra_builder_output):
    return example_hydra_builder_output[None]

@pytest.fixture(scope="module")
def example_hydra_results():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample2, globalName="GetGitInfo")
        starttime = datetime.now()
        inp_desc, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                             actor_system=asys,
                                                             verbose=True)
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        config_results = bcgen.generate(inp_desc, repo_info)
        builder_cfgs, build_cfgs = config_results
        anarep = AnaRep.AnaRep(verbose=True, actor_system=asys)
        # n.b. the name values for build_results come from
        # builder._jobset_name, which is revealed by this print loop.
        for each in build_cfgs.cfg_build_configs:
            print(builder._jobset_name(each))
        builder._build_results = [
            { "name" : "develop.HEADs-ghc844",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "develop.HEADs-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 0,
              "nrscheduled": 1,
              "haserrormsg": False,
            },
            { "name" : "develop.HEADs-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "develop.submodules-ghc844",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": True,
            },
            { "name" : "develop.submodules-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "develop.submodules-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.HEADs-ghc844",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.HEADs-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.HEADs-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.submodules-ghc844",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.submodules-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.submodules-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
        ]
        prior = [
            StatusReport(status='initial_success', project='Repo1',
                         strategy='submodules', branchtype="regular", branch="master",
                         buildname='master.submodules-ghc844',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc844')
                         ]),
            StatusReport(status='failed', project='Repo1',
                         strategy='HEADs', branchtype="regular", branch="master",
                         buildname='master.HEADs-ghc865',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                         ]),
            StatusReport(status='succeeded', project='Repo1',
                         strategy='HEADs', branchtype="regular", branch="master",
                         buildname='master.HEADs-ghc881',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                         ]),

        ]
        report = anarep.report_on([AnaRep.ResultSet(builder, inp_desc, repo_info, build_cfgs)], prior)
        assert report[0] == 'report'
        endtime = datetime.now()
        # This should be a proper test: checks the amount of time to run run the logic process.
        assert endtime - starttime < timedelta(seconds=1, milliseconds=500)  # avg 1.02s
        yield (builder_cfgs, report[1])
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
:- discontiguous project/1.
:- discontiguous repo/1.
:- discontiguous subrepo/1.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous pullreq/3.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("Repo1").
repo("Repo1").
repo("Repo2").
repo("Repo3").
subrepo("Repo2").
subrepo("Repo3").
subrepo("Repo4").
main_branch("Repo1", "master").
main_branch("Repo2", "master").
main_branch("Repo3", "master").
branchreq("Repo1", "master").
branchreq("Repo1", "develop").
branch("Repo3", "develop").
branch("Repo4", "master").
branch("Repo1", "develop").
branch("Repo2", "master").
branch("Repo3", "master").
branch("Repo2", "develop").
branch("Repo1", "master").
submodule("Repo1", project_primary, "develop", "Repo2", "r2_develop_head").
submodule("Repo1", project_primary, "develop", "Repo3", "r3_develop_head").
submodule("Repo1", project_primary, "develop", "Repo4", "r4_master_head").
submodule("Repo1", project_primary, "master", "Repo2", "r2_master_head").
submodule("Repo1", project_primary, "master", "Repo3", "r3_master_head^3").
submodule("Repo1", project_primary, "master", "Repo4", "r4_master_head^1").
varname("Repo1", "ghcver").
varvalue("Repo1", "ghcver", "ghc844").
varvalue("Repo1", "ghcver", "ghc865").
varvalue("Repo1", "ghcver", "ghc881").
'''.split('\n')))


def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))

def test_example_internal_regular_develop_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "submodules",
                            [
                                BldRepoRev("Repo1", "develop", "project_primary"),
                                BldRepoRev("Repo2", "r2_develop_head", "project_primary"),
                                BldRepoRev("Repo3", "r3_develop_head", "project_primary"),
                                BldRepoRev("Repo4", "r4_master_head", "project_primary"),
                            ],
                            [
                                BldVariable("Repo1", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "HEADs",
                            [
                                BldRepoRev("Repo1", "develop", "project_primary"),
                                BldRepoRev("Repo2", "develop", "project_primary"),
                                BldRepoRev("Repo3", "develop", "project_primary"),
                                BldRepoRev("Repo4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("Repo1", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_submodules(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "submodules",
                            [
                                BldRepoRev("Repo1", "master", "project_primary"),
                                BldRepoRev("Repo2", "r2_master_head", "project_primary"),
                                BldRepoRev("Repo3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("Repo4", "r4_master_head^1", "project_primary"),
                            ],
                            [
                                BldVariable("Repo1", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_HEADs(example_internal_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "HEADs",
                            [
                                BldRepoRev("Repo1", "master", "project_primary"),
                                BldRepoRev("Repo2", "master", "project_primary"),
                                BldRepoRev("Repo3", "master", "project_primary"),
                                BldRepoRev("Repo4", "master", "project_primary"),
                            ],
                            [
                                BldVariable("Repo1", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs



def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))


def test_example_report_summary(example_hydra_results):
    bldcfgs, reps = example_hydra_results

    for each in reps:
        print('')
        print(each)

    # Note that Repo2, Repo3, and Repo4 are all subrepos of Repo1, even though
    # they are also explicitly listed in the input specification.
    assert ProjectSummary(project_name='Repo1',
                          bldcfg_count=12, subrepo_count=3, pullreq_count=0) in reps

def test_example_report_status1(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="develop",
                        buildname='develop.HEADs-ghc844',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc844')
                        ]) in reps

# def test_example_report_status2(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one is pending:
#     assert StatusReport(status='initial_success', project='Repo1',
#                         strategy='HEADs', branchtype="regular", branch="develop",
#                         buildname='develop.HEADs-ghc865',
#                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
#                         ]) in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="develop",
                        buildname='develop.HEADs-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

# def test_example_report_status4(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one has an error message (configInvalid)
#     assert StatusReport(status='initial_success', project='Repo1',
#                         strategy='submodules', branchtype="regular", branch="master",
#                         buildname='develop.submodules-ghc844',
#                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc844')
#                         ]) in reps

def test_example_report_status5(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="develop",
                        buildname='develop.submodules-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

def test_example_report_status6(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="develop",
                        buildname='develop.submodules-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status7(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc844',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc844')
                        ]) in reps

def test_example_report_status8(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one had a prior failure
    assert StatusReport(status='fixed', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

def test_example_report_status9(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one had a prior success
    assert StatusReport(status='failed', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status10(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one has a prior success
    assert StatusReport(status='succeeded', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc844',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc844')
                        ]) in reps

def test_example_report_status11(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

def test_example_report_status12(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_ghc881_varfailure(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert VarFailure('Repo1', 'ghcver', 'ghc881') in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    nrscheduled = 1
    expected = ((len(top_level) * len(GS)) +
                len(['ProjectSummary', 'VarFailure', 'ConfigError'])
                - nrscheduled)
    assert expected == len(reps)
