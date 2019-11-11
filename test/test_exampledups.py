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
        anarep = AnaRep.AnaRep(verbose=True, actor_system=asys)
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
            { "name" : "PR1-master.main-ghc865",
              "nrtotal" : 4,
              "nrsucceeded": 4,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR1-master.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR2-develop.main-ghc865",
              "nrtotal" : 0,
              "nrsucceeded": 0,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": True,
            },
            { "name" : "PR2-develop.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR1-foo.main-ghc865",
              "nrtotal" : 9,
              "nrsucceeded": 9,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR1-foo.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR9-master.main-ghc865",
              "nrtotal" : 9,
              "nrsucceeded": 9,
              "nrfailed": 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
            { "name" : "PR9-master.main-ghc881",
              "nrtotal" : 4,
              "nrsucceeded": 3,
              "nrfailed": 1,
              "nrscheduled": 0,
              "haserrormsg": False,
            },
        ]
        prior = [
            StatusReport(status='initial_success', project='Repo1',
                         strategy='main', branchtype="pullreq", branch="foo",
                         buildname='PR1-foo.main-ghc881',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                         ]),
            StatusReport(status='initial_success', project='Repo1',
                         strategy='main', branchtype="pullreq", branch="foo",
                         buildname='PR1-foo.main-ghc865',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                         ]),
            StatusReport(status='failed', project='Repo1',
                         strategy='main', branchtype="pullreq", branch="master",
                         buildname='PR1-master.main-ghc865',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                         ]),
            StatusReport(status='succeeded', project='Repo1',
                         strategy='main', branchtype="regular", branch="master",
                         buildname='master.main-ghc881',
                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                         ]),
        ]
        report = anarep.report_on([AnaRep.ResultSet(builder, inp_desc, repo_info, build_cfgs)], prior)
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
    "pullreq R3 master",
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
:- discontiguous project/1.
:- discontiguous repo/1.
:- discontiguous subrepo/1.
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
branchreq("Repo1", "master").
branchreq("Repo1", "develop").
pullreq("Repo1", "1", "master").
pullreq("Repo3", "2", "develop").
pullreq("Repo3", "1", "foo").
pullreq("Repo3", "9", "master").
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
                            blds=[BldRepoRev("Repo1", "master", "project_primary"),
                                  BldRepoRev("Repo2", "master", "project_primary"),
                                  BldRepoRev("Repo3", "master", "project_primary"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="regular",
                            branchname="develop",
                            strategy="main",
                            blds=[BldRepoRev("Repo1", "develop", "project_primary"),
                                  BldRepoRev("Repo2", "develop", "project_primary"),
                                  BldRepoRev("Repo3", "develop", "project_primary"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr1_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="master",
                            strategy="main",
                            blds=[BldRepoRev("Repo1", "master", "1"),
                                  BldRepoRev("Repo2", "master", "project_primary"),
                                  BldRepoRev("Repo3", "master", "project_primary"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr2_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="develop",
                            strategy="main",
                            blds=[BldRepoRev("Repo1", "develop", "project_primary"),
                                  BldRepoRev("Repo2", "develop", "project_primary"),
                                  BldRepoRev("Repo3", "develop", "2"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr1r3_main(example_internal_bldconfigs):
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="foo",
                            strategy="main",
                            blds=[BldRepoRev("Repo1", "master", "project_primary"),
                                  BldRepoRev("Repo2", "foo", "project_primary"),
                                  BldRepoRev("Repo3", "foo", "1"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs

def test_example_internal_pr9r3_main(example_internal_bldconfigs):
    for each in example_internal_bldconfigs.cfg_build_configs:
        print(each)
        print('')
    for each in [ BldConfig(projectname="Repo1",
                            branchtype="pullreq",
                            branchname="master",
                            strategy="main",
                            blds=[BldRepoRev("Repo1", "master", "project_primary"),
                                  BldRepoRev("Repo2", "master", "project_primary"),
                                  BldRepoRev("Repo3", "master", "9"),
                            ],
                            bldvars=[BldVariable("Repo1", "ghcver", G)])
                  for G in GS]:
        assert each in example_internal_bldconfigs.cfg_build_configs


def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

def test_example_hydra_master(example_hydra_jobsets):
    expected = dict([
        ( "master.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:Repo1, brr1:Repo2, brr1:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url master"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=master|strategy=main"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_develop(example_hydra_jobsets):
    expected = dict([
        ( "develop.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:Repo1, brr1:Repo2, brr1:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url develop"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url develop"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url develop"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=develop|strategy=main"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_R1PR1(example_hydra_jobsets):
    expected = dict([
        ( "PR1-master.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: PR1-brr3:Repo1, brr1:Repo2, brr1:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_Repo1 master"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url master"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r3_url master"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=master|strategy=main|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_R3PR1(example_hydra_jobsets):
    expected = dict([
        ( "PR1-foo.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr2:Repo1, brr1:Repo2, PR1-brr3:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url foo"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_Repo3_2 foo"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=foo|strategy=main|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_R3PR2(example_hydra_jobsets):
    expected = dict([
        ( "PR2-develop.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:Repo1, brr1:Repo2, PR2-brr3:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url develop"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url develop"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_Repo3 develop"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=develop|strategy=main|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]

def test_example_hydra_R3PR9(example_hydra_jobsets):
    expected = dict([
        ( "PR9-master.main-%s" % (G), {
            "checkinterval": 600,
            "description": "Build configuration: brr1:Repo1, brr1:Repo2, PR9-brr3:Repo3, ghcver=%s" % (G),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "inputs": {
                "Repo1-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r1_url master"
                },
                "Repo2-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "r2_url master"
                },
                "Repo3-src": {
                    "emailresponsible": False,
                    "type": "git",
                    "value": "remote_repo3_other master"
                },
                "ghcver": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": G
                },
                "variant": {
                    "emailresponsible": False,
                    "type": "string",
                    "value": "|branch=master|strategy=main|PR"
                },
            },
            "keepnr": 3,
            "nixexprinput": "Repo1-src",
            "nixexprpath": "./release.nix",
            "schedulingshares": 1
        }) for G in GS ])
    for each in expected:
        print(each)
        actual = json.loads(example_hydra_jobsets)
        assert each in actual
        assert expected[each] == actual[each]



def test_example_report_summary(example_hydra_results):
    bldcfgs, reps = example_hydra_results

    for each in reps:
        print('')
        print(each)

    assert ProjectSummary(project_name='Repo1',
                          bldcfg_count=12, subrepo_count=0, pullreq_count=4) in reps

# def test_example_report_status1(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one has a bad configuration
#     assert StatusReport(status='failed', project='Repo1',
#                         strategy='main', branchtype="pullreq", branch="develop",
#                         buildname='PR-develop.main-ghc865',
#                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
#                         ]) in reps

def test_example_report_status2(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', branchtype="pullreq", branch="develop",
                        buildname='PR2-develop.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', branchtype="pullreq", branch="foo",
                        buildname='PR1-foo.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status4(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', branchtype="pullreq", branch="master",
                        buildname='PR1-master.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status5(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', branchtype="regular", branch="develop",
                        buildname='develop.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status6(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='failed', project='Repo1',
                        strategy='main', branchtype="regular", branch="master",
                        buildname='master.main-ghc881',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc881')
                        ]) in reps

def test_example_report_status7(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='main', branchtype="pullreq", branch="master",
                        buildname='PR9-master.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

def test_example_report_status8(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='main', branchtype="regular", branch="master",
                        buildname='master.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

def test_example_report_status9(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='succeeded', project='Repo1',
                        strategy='main', branchtype="pullreq", branch="foo",
                        buildname='PR1-foo.main-ghc865',
                        bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
                        ]) in reps

# def test_example_report_status10(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one is in-progress
#     assert StatusReport(status='succeeded', project='Repo1',
#                         strategy='main', branchtype="regular", branch="develop",
#                         buildname='develop.main-ghc865',
#                         bldvars=[BldVariable(projrepo='Repo1', varname='ghcver', varvalue='ghc865')
#                         ]) in reps

def test_example_report_varfailure(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert VarFailure('Repo1', 'ghcver', 'ghc881') in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert (len(GS) * len(top_level) - 2 + 2) == len(reps)
