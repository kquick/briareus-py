import Briareus.AnaRep.Operations as AnaRep
import Briareus.BCGen.Operations as BCGen
from Briareus.Types import (BldConfig, BldRepoRev, BldVariable,
                            ProjectSummary, StatusReport, VarFailure)
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
from git_exampledups import GitExample
import json
import pytest

# This test is similar to the small test_example2, except:
#
#  * There are PR's and requested branches with the same names.
#
#  * There are no submodules
#
# This test suite ensures that the PR's do not mask the requested
# branches, and vice-versa.  This test suite also validates operation
# without submodules.
#
# A Pull request or merge request comes from a separate repository and
# should therefore be treated as effectively a *different* branch than
# a local branch.  If a PR has the same name as a local branch in a
# specific repository, that should result in two separate build
# configurations.
#


input_spec = '''
{
  "Repos" : [ ("Repo1", "r1_url"),
              ("Repo2", "r2_url"),
              ("Repo3", "r3_url"),
            ]
, "Branches" : [ "master", "develop" ]
, "Variables" : {
      "ghcver" : [ "ghc865", "ghc881" ],
  }
}
'''


@pytest.fixture(scope="module")
def example_internal_bldconfigs():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample, globalName="GetGitInfo")
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
        asys.createActor(GitExample, globalName="GetGitInfo")
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
def example_hydra_results():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample, globalName="GetGitInfo")
        inp_desc, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                             actor_system=asys,
                                                             verbose=True)
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        config_results = bcgen.generate(inp_desc, repo_info)
        builder_cfgs, build_cfgs = config_results
        anarep = AnaRep.AnaRep(builder, verbose=True, actor_system=asys)
        # n.b. the name values for build_results come from
        # builder._jobset_name, which is revealed by this print loop.
        for each in build_cfgs.cfg_build_configs:
            print(builder._jobset_name(each))
        builder._build_results = [
            { "name" : "develop.main-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 0,
              "nrscheduled": 1,
              "haserrormsg": False,
            },
            { "name" : "develop.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.main-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "master.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR-master.main-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR-master.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR-develop.main-ghc865",
              "nrtotal" : 0,
              "nrsucceeded": 0,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": True,
            },
            { "name" : "PR-develop.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR-foo.main-ghc865",
              "nrtotal" : 9,
              "nrsucceeded": 9,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR-foo.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
        ]
        prior = [
            StatusReport(status='initial_success', project='Repo1',
                         strategy='main', buildname='PR-foo.main-ghc881',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                         ]),
            StatusReport(status='initial_success', project='Repo1',
                         strategy='main', buildname='PR-foo.main-ghc865',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                         ]),
            StatusReport(status='failed', project='Repo1',
                         strategy='main', buildname='PR-master.main-ghc865',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                         ]),
            StatusReport(status='succeeded', project='Repo1',
                         strategy='main', buildname='master.main-ghc881',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                         ]),
        ]
        report = anarep.report_on(inp_desc, repo_info, build_cfgs, prior)
        assert report[0] == 'report'
        yield (builder_cfgs, report[1])
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

GS = [ "ghc865", "ghc881" ]
top_level = [
    "regular develop main",
    "regular master main",
    "pullreq R1 master",
    "pullreq R3 develop",
    "pullreq R3 foo",
]

def test_example_facts():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample, globalName="GetGitInfo")
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
:- dynamic varvalue/3.
project("Repo1").
repo("Repo1").
repo("Repo2").
repo("Repo3").
branchreq("Repo1", "master").
branchreq("Repo1", "develop").
pullreq("Repo1", "1", "master").
pullreq("Repo3", "2", "develop").
pullreq("Repo3", "1", "foo").
branch("Repo3", "develop").
branch("Repo1", "develop").
branch("Repo2", "master").
branch("Repo3", "master").
branch("Repo2", "develop").
branch("Repo1", "master").
branch("Repo2", "foo").
varname("Repo1", "ghcver").
varvalue("Repo1", "ghcver", "ghc865").
varvalue("Repo1", "ghcver", "ghc881").
'''.split('\n')))


def test_example_internal_count(example_internal_bldconfigs):
    assert len(GS) * len(top_level) == len(set(example_internal_bldconfigs.cfg_build_configs))


def test_example_internal_regular_master_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="regular",
                            branchname="master",
                            strategy="main",
                            blds=[BldRepoRev("Repo1","master"),
                                  BldRepoRev("Repo2","master"),
                                  BldRepoRev("Repo3","master"),
                            ],
                            bldvars=[BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="regular",
                            branchname="develop",
                            strategy="main",
                            blds=[BldRepoRev("Repo1","develop"),
                                  BldRepoRev("Repo2","develop"),
                                  BldRepoRev("Repo3","develop"),
                            ],
                            bldvars=[BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr1_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="master",
                            strategy="main",
                            blds=[BldRepoRev("Repo1","master"),
                                  BldRepoRev("Repo2","master"),
                                  BldRepoRev("Repo3","master"),
                            ],
                            bldvars=[BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr2_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="develop",
                            strategy="main",
                            blds=[BldRepoRev("Repo1","develop"),
                                  BldRepoRev("Repo2","develop"),
                                  BldRepoRev("Repo3","develop"),
                            ],
                            bldvars=[BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr1r3_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="foo",
                            strategy="main",
                            blds=[BldRepoRev("Repo1","master"),
                                  BldRepoRev("Repo2","foo"),
                                  BldRepoRev("Repo3","foo"),
                            ],
                            bldvars=[BldVariable("Repo1","ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))


def test_example_report(example_hydra_results):
    bldcfgs, reps = example_hydra_results

    for each in reps:
        print('')
        print(each)

    assert ProjectSummary(project_name='Repo1',
                          bldcfg_count=10, subrepo_count=0, pullreq_count=3) in reps

    # This one has a bad configuration
    # assert StatusReport(status='failed', project='Repo1',
    #                     strategy='main', buildname='PR-develop.main-ghc865',
    #                     bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
    #                     ]) in reps

    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', buildname='PR-develop.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', buildname='PR-foo.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', buildname='PR-master.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', buildname='develop.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', buildname='master.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

    assert StatusReport(status='fixed', project='Repo1',
                        strategy='main', buildname='PR-master.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='main', buildname='master.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

    assert StatusReport(status='succeeded', project='Repo1',
                        strategy='main', buildname='PR-foo.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

    # This one is in-progress
    # assert StatusReport(status='succeeded', project='Repo1',
    #                     strategy='main', buildname='develop.main-ghc865',
    #                     bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
    #                     ]) in reps

    assert VarFailure('Repo1', 'ghcver', 'ghc881') in reps

    assert (len(GS) * len(top_level) - 2 + 2) == len(reps)
