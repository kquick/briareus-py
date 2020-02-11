from Briareus.Types import (BldConfig, BldRepoRev, BldVariable, BranchReq,
                            ProjectSummary, StatusReport, VarFailure)
from git_example2 import GitExample2
import json
import pytest
from datetime import timedelta

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
, "Reporting" : {
      "logic": """
project_owner("Repo1", "george@_company.com").

project_owner("Repo3", "john@not_a_company.com").

action_type(email, "fred@nocompany.com", "Repo1").
action_type(email, "anne@nocompany.com", "Repo1", master_submodules_broken).
      """
  }
}
'''

gitactor = GitExample2


@pytest.fixture(scope="module")
def example_hydra_jobsets(generated_hydra_builder_output):
    return generated_hydra_builder_output[0][None]

build_results = [
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
                 bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
                 ],
                 blddesc=BranchReq('Repo1', 'master'),
    ),
    StatusReport(status=1, project='Repo1',
                 strategy='HEADs', branchtype="regular", branch="master",
                 buildname='master.HEADs-ghc865',
                 bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc865')
                 ],
                 blddesc=BranchReq('Repo1', 'master'),
    ),
    StatusReport(status='succeeded', project='Repo1',
                 strategy='HEADs', branchtype="regular", branch="master",
                 buildname='master.HEADs-ghc881',
                 bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc881')
                 ],
                 blddesc=BranchReq('Repo1', 'master'),
    ),
]

analysis_time_budget = timedelta(seconds=1, milliseconds=500)  # avg 1.02s


analysis_time_budget = timedelta(seconds=1, milliseconds=500)  # avg 1.02s

GS = [ "ghc844", "ghc865", "ghc881" ]
top_level = [
    "regular develop HEADs",
    "regular develop submodules",
    "regular master HEADs",
    "regular master submodules",
]

def test_example_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))

expected_facts = sorted(filter(None, '''
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous subrepo/2.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous pullreq/3.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("Repo1", "Repo1").
repo("Repo1", "Repo1").
repo("Repo1", "Repo2").
repo("Repo1", "Repo3").
subrepo("Repo1", "Repo2").
subrepo("Repo1", "Repo3").
subrepo("Repo1", "Repo4").
default_main_branch("master").
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


def test_example_internal_count(generated_bldconfigs):
    assert len(GS) * len(top_level) == len(generated_bldconfigs.cfg_build_configs)

def test_example_internal_regular_develop_submodules(generated_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "submodules",
                            BranchReq("Repo1", "develop"),
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
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_regular_develop_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "develop", "HEADs",
                            BranchReq("Repo1", "develop"),
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
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_submodules(generated_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "submodules",
                            BranchReq("Repo1", "master"),
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
        assert each in generated_bldconfigs.cfg_build_configs

def test_example_internal_regular_master_HEADs(generated_bldconfigs):
    for each in [ BldConfig("Repo1", "regular", "master", "HEADs",
                            BranchReq("Repo1", "master"),
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
        assert each in generated_bldconfigs.cfg_build_configs



def test_example_hydra_count(example_hydra_jobsets):
    print('##### OUTPUT:')
    print(example_hydra_jobsets)
    assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

@pytest.fixture(scope="module")
def example_hydra_results(generate_hydra_results):
    return generate_hydra_results(build_results=build_results, prior=prior)

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
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
                        ],
                        blddesc=BranchReq('Repo1', 'develop'),
    ) in reps

# def test_example_report_status2(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one is pending:
#     assert StatusReport(status='initial_success', project='Repo1',
#                         strategy='HEADs', branchtype="regular", branch="develop",
#                         buildname='develop.HEADs-ghc865',
#                         bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc865')
#                         ],
#                         blddesc=BranchReq('Repo1', 'develop'),
#    ) in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status=1, project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="develop",
                        buildname='develop.HEADs-ghc881',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc881')
                        ],
                        blddesc=BranchReq('Repo1', 'develop'),
    ) in reps

# def test_example_report_status4(example_hydra_results):
#     bldcfgs, reps = example_hydra_results
#     # This one has an error message (configInvalid)
#     assert StatusReport(status='initial_success', project='Repo1',
#                         strategy='submodules', branchtype="regular", branch="master",
#                         buildname='develop.submodules-ghc844',
#                         bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
#                         ],
#                         blddesc=BranchReq('Repo1', 'master'),
#    ) in reps

def test_example_report_status5(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="develop",
                        buildname='develop.submodules-ghc865',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc865')
                        ],
                 blddesc=BranchReq('Repo1', 'develop'),
    ) in reps

def test_example_report_status6(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status=1, project='Repo1',
                        strategy='submodules', branchtype="regular", branch="develop",
                        buildname='develop.submodules-ghc881',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc881')
                        ],
                 blddesc=BranchReq('Repo1', 'develop'),
    ) in reps

def test_example_report_status7(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc844',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
                        ],
                 blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status8(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one had a prior failure
    assert StatusReport(status='fixed', project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc865',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status9(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one had a prior success
    assert StatusReport(status=1, project='Repo1',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc881',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc881')
                        ],
                        blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status10(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # This one has a prior success
    assert StatusReport(status='succeeded', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc844',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
                        ],
                        blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status11(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='initial_success', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status12(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status=1, project='Repo1',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc881',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc881')
                        ],
                        blddesc=BranchReq('Repo1', 'master'),
    ) in reps

def test_example_report_status13(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='bad_config', project='Repo1',
                        strategy='submodules', branchtype="regular", branch="develop",
                        buildname='develop.submodules-ghc844',
                        bldvars=[BldVariable(project='Repo1', varname='ghcver', varvalue='ghc844')
                        ],
                        blddesc=BranchReq('Repo1', 'develop'),
    ) in reps

def test_example_report_ghc881_varfailure(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert VarFailure('Repo1', 'ghcver', 'ghc881') in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    num_varfailure = 1
    pr_status = 0
    num_notify = num_varfailure + pr_status
    num_actions = 0  # no enables
    expected = ((len(top_level) * len(GS)) +
                len(['ProjectSummary', ])
                + (num_varfailure * 2)  # VarFailure + SepHandledVar
                + pr_status
                + num_notify
                + num_actions)
    for each in reps:
        print(each)
        print('')
    assert expected == len(reps)
