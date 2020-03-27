from Briareus.Types import (BldConfig, BldRepoRev, BldVariable, BranchReq,
                            ProjectSummary, StatusReport, VarFailure,
                            Notify, SendEmail)
from git_example2 import GitExample2
import Briareus.hh as hh
import test_example2 as tex2
import json
import pytest
from datetime import datetime, timedelta

# This test is an extension of test_example2, with a different project
# specification using the same Repo1 as the top-level to get the
# submodules specification from Repo1, but extending that with some
# additional repos.  It also uses only a subset of the variables. The
# two projects are configured together as a single inp_config so that
# both are seen at the same time.  The tests to run for this are to
# ensure that the two projects are treated distinctly and that the
# proper logic is applied to each project.


input_spec = '''
{
  "Repos" : [ ("Repo1", "r1_url"),
              ("RAdd1", "ra1_url"),
              ("Repo3", "r3_url"),
              ("RAdd2", "ra2_url"),
            ]
, "Name": "alt-proj"
, "Branches" : [ "master" ]
, "Variables" : {
      "ghcver" : [ "ghc865" ],
  }
, "Reporting" : {
      "logic": """
project_owner("alt-proj", "julio@uptown.funk").
      """
  }
}
'''

gitactor = GitExample2

@pytest.fixture(scope="session")
def testing_dir(tmpdir_factory):
    return tmpdir_factory.mktemp("ex2_add")

@pytest.fixture(scope="session")
def ex2_hhd(testing_dir):
    hhd = testing_dir.join("example2.hdd")
    hhd.write(tex2.input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="session")
def ex2add_hhd(testing_dir):
    hhd = testing_dir.join("example2add.hdd")
    hhd.write(input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="module")
def inp_configs(testing_dir, ex2_hhd, ex2add_hhd):
    outfile_ex2 = testing_dir.join("ex2.hhc")
    outfile_ex2add = testing_dir.join("ex2add.hhc")
    return [
        (GitExample2, outfile_ex2,
         hh.InpConfig(hhd=ex2_hhd, builder_type="hydra", output_file=outfile_ex2),
         ),
        (GitExample2, outfile_ex2add,
         hh.InpConfig(hhd=ex2add_hhd, builder_type="hydra", output_file=outfile_ex2add),
         ),
    ]

build_results = [
    { "name" : "master.HEADs-ghc865",
      "nrtotal" : 4,
      "nrsucceeded": 4,
      "nrfailed": 0,
      "nrscheduled": 0,
      "haserrormsg": False,
    },
    { "name" : "master.submodules-ghc865",
      "nrtotal" : 4,
      "nrsucceeded": 3,
      "nrfailed": 1,
      "nrscheduled": 0,
      "haserrormsg": False,
    },
]

prior = [
    SendEmail(["julio@uptown.funk"],
              Notify("master_submodules_broken", "alt-proj", ["master.submodules-ghc865"]),
              ["julio@uptown.funk"]),
    StatusReport(status=1, project='alt-proj',
                 strategy='HEADs', branchtype="regular", branch="master",
                 buildname='master.HEADs-ghc865',
                 bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                 ],
                 blddesc=BranchReq('alt-proj', 'master'),
    ),
]

analysis_time_budget = timedelta(seconds=1, milliseconds=500)  # avg 1.02s


analysis_time_budget = timedelta(seconds=1, milliseconds=500)  # avg 1.02s

GS = [ "ghc865", ]
top_level = [
    "regular master HEADs",
    "regular master submodules",
]

def test_example_facts(generated_inp_config_facts):
    assert expected_facts == sorted(list(map(str, generated_inp_config_facts)))

expected_facts = sorted(filter(None, '''
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous subrepo/2.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous pullreq/6.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("alt-proj", "Repo1").
repo("alt-proj", "Repo1").
repo("alt-proj", "RAdd1").
repo("alt-proj", "RAdd2").
repo("alt-proj", "Repo3").
subrepo("Repo1", "Repo2").
subrepo("Repo1", "Repo3").
subrepo("Repo1", "Repo4").
default_main_branch("master").
branchreq("alt-proj", "master").
branch("Repo4", "master").
branch("Repo2", "master").
branch("Repo3", "master").
branch("Repo1", "master").
branch("RAdd1", "master").
branch("RAdd2", "master").
submodule("Repo1", project_primary, "master", "Repo2", "r2_master_head").
submodule("Repo1", project_primary, "master", "Repo3", "r3_master_head^3").
submodule("Repo1", project_primary, "master", "Repo4", "r4_master_head^1").
varname("alt-proj", "ghcver").
varvalue("alt-proj", "ghcver", "ghc865").
'''.split('\n') + tex2.expected_facts))


def test_example_internal_count_add(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "alt-proj" == Res.inp_desc.PNAME ][0]
    assert len(GS) * len(top_level) == len(exp_results.build_cfgs.cfg_build_configs)

def test_example_internal_count_main(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "Repo1" == Res.inp_desc.PNAME ][0]
    assert len(tex2.GS) * len(tex2.top_level) == len(exp_results.build_cfgs.cfg_build_configs)

def test_example_internal_add_regular_master_submodules(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "alt-proj" == Res.inp_desc.PNAME ][0]
    for each in [ BldConfig("alt-proj", "regular", "master", "submodules",
                            BranchReq("alt-proj", "master"),
                            [
                                BldRepoRev("Repo1", "master", "project_primary"),
                                BldRepoRev("Repo2", "r2_master_head", "project_primary"),
                                BldRepoRev("Repo3", "r3_master_head^3", "project_primary"),
                                BldRepoRev("Repo4", "r4_master_head^1", "project_primary"),
                                BldRepoRev("RAdd1", "master",  "project_primary"),
                                BldRepoRev("RAdd2", "master",  "project_primary"),
                            ],
                            [
                                BldVariable("alt-proj", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in exp_results.build_cfgs.cfg_build_configs

def test_example_internal_add_regular_master_HEADs(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "alt-proj" == Res.inp_desc.PNAME ][0]
    for each in [ BldConfig("alt-proj", "regular", "master", "HEADs",
                            BranchReq("alt-proj", "master"),
                            [
                                BldRepoRev("Repo1", "master", "project_primary"),
                                BldRepoRev("Repo2", "master", "project_primary"),
                                BldRepoRev("Repo3", "master", "project_primary"),
                                BldRepoRev("Repo4", "master", "project_primary"),
                                BldRepoRev("RAdd1", "master",  "project_primary"),
                                BldRepoRev("RAdd2", "master",  "project_primary"),
                            ],
                            [
                                BldVariable("alt-proj", "ghcver", G),
                            ])
                  for G in GS]:
        assert each in exp_results.build_cfgs.cfg_build_configs

def test_example_internal_regular_develop_submodules(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "Repo1" == Res.inp_desc.PNAME ][0]
    tex2.test_example_internal_regular_develop_submodules(exp_results.build_cfgs)

def test_example_internal_regular_develop_HEADs(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "Repo1" == Res.inp_desc.PNAME ][0]
    tex2.test_example_internal_regular_develop_HEADs(exp_results.build_cfgs)

def test_example_internal_regular_master_submodules(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "Repo1" == Res.inp_desc.PNAME ][0]
    tex2.test_example_internal_regular_master_submodules(exp_results.build_cfgs)

def test_example_internal_regular_master_HEADs(generated_inp_config_bldconfigs):
    exp_results = [ Res
                    for Res in generated_inp_config_bldconfigs.result_sets
                    if "Repo1" == Res.inp_desc.PNAME ][0]
    tex2.test_example_internal_regular_master_HEADs(exp_results.build_cfgs)

######################################################################

# @pytest.fixture(scope="module")
# def example_hydra_jobsets(generated_hydra_builder_output):  #KWQ?
#     return generated_hydra_builder_output[0][None]
# def test_example_hydra_count(example_hydra_jobsets):
#     print('##### OUTPUT:')
#     print(example_hydra_jobsets)
#     assert len(GS) * len(top_level) == len(json.loads(example_hydra_jobsets))

######################################################################
# Test singular results as if it was only alt-proj

@pytest.fixture(scope="module")
def example_hydra_results(generate_hydra_results):
    r = generate_hydra_results(build_results=build_results, prior=prior)
    return r[0], r[1].report

def test_example_report_summary(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Note that Repo2, Repo3, and Repo4 are all subrepos of Repo1, even though
    # they are also explicitly listed in the input specification.
    assert ProjectSummary(project_name='alt-proj',
                          bldcfg_count=2, subrepo_count=3, pullreq_count=0) in reps

def test_example_report_status1(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status='fixed', project='alt-proj',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status=1, project='alt-proj',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_example_report_send_email(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert StatusReport(status=1, project='alt-proj',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    num_varfailure = 0
    pr_status = 0
    num_notify = num_varfailure + pr_status + 1 # main_submodules_broken
    num_actions = 1  # main_submodules_good -> repo owner
    expected = ((len(top_level) * len(GS)) +   # 4 * 3
                len(['ProjectSummary', ])
                + (num_varfailure * 2)  # VarFailure + SepHandledVar
                + pr_status
                + num_notify
                + num_actions)
    assert expected == len(reps)


######################################################################
# Test combined results of both projects

@pytest.fixture(scope="module")
def example_report(testing_dir, generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        each.builder._build_results = "project name", {
            "Repo1": tex2.build_results,
            "alt-proj": build_results,
        }[each.inp_desc.PNAME]
    return generate_report(testing_dir, generated_inp_config_bldconfigs, tex2.prior + prior)

def generate_report(testdir, inp_config_bldconfigs, prior, reporting_logic_defs=''):
    params = hh.Params(verbose=True, up_to=None,
                       report_file=testdir.join("ex2_add.hhr"))
    starttime = datetime.now()
    rep = hh.run_hh_report(params, inp_config_bldconfigs, prior,
                           reporting_logic_defs=reporting_logic_defs)
    endtime = datetime.now()
    # This should be a proper test: checks the amount of time to run run the logic process.
    assert endtime - starttime < timedelta(seconds=2, milliseconds=500)  # avg 1.02s
    return rep

def test_combined_report_summary(example_report):
    reps = example_report.report
    for each in reps:
        print(str(each))
        print()
    assert ProjectSummary(project_name='Repo1+alt-proj',
                          bldcfg_count=14, subrepo_count=6, pullreq_count=0) in reps

def test_combined_report_add_status1(example_report):
    reps = example_report.report
    assert StatusReport(status='fixed', project='alt-proj',
                        strategy='HEADs', branchtype="regular", branch="master",
                        buildname='master.HEADs-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_combined_report_add_status3(example_report):
    reps = example_report.report
    assert StatusReport(status=1, project='alt-proj',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_combined_report_add_send_email(example_report):
    reps = example_report.report
    assert StatusReport(status=1, project='alt-proj',
                        strategy='submodules', branchtype="regular", branch="master",
                        buildname='master.submodules-ghc865',
                        bldvars=[BldVariable(project='alt-proj', varname='ghcver', varvalue='ghc865')
                        ],
                        blddesc=BranchReq('alt-proj', 'master'),
    ) in reps

def test_combined_report_status1(example_report):
    tex2.test_example_report_status1((None, example_report.report))

def test_combined_report_status3(example_report):
    tex2.test_example_report_status3((None, example_report.report))

def test_combined_report_status5(example_report):
    tex2.test_example_report_status5((None, example_report.report))

def test_combined_report_status6(example_report):
    tex2.test_example_report_status6((None, example_report.report))

def test_combined_report_status7(example_report):
    tex2.test_example_report_status7((None, example_report.report))

def test_combined_report_status8(example_report):
    tex2.test_example_report_status8((None, example_report.report))

def test_combined_report_status9(example_report):
    tex2.test_example_report_status9((None, example_report.report))

def test_combined_report_status10(example_report):
    tex2.test_example_report_status10((None, example_report.report))

def test_combined_report_status11(example_report):
    tex2.test_example_report_status11((None, example_report.report))

def test_combined_report_status12(example_report):
    tex2.test_example_report_status12((None, example_report.report))

def test_combined_report_status13(example_report):
    tex2.test_example_report_status13((None, example_report.report))

def test_combined_report_ghc881_varfailure(example_report):
    tex2.test_example_report_ghc881_varfailure((None, example_report.report))

def test_combined_report_length(example_report):
    reps = example_report.report
    num_varfailure = 1 + 0
    pr_status = 0 + 0
    num_notify = (num_varfailure + pr_status +
                  1 + # main_submodules_broken
                  1   # main_submodules_good
                  )
    num_actions = (1 +  # main_submodules_good -> repo owner
                   1    # main submodules broken -> repo owner
    )
    expected = ((len(top_level) * len(GS)) +   # 2 * 1
                (len(tex2.top_level) * len(tex2.GS)) +   # 4 * 3
                len(['ProjectSummary', ])
                + (num_varfailure * 2)  # VarFailure + SepHandledVar
                + pr_status
                + num_notify
                + num_actions)
    for each in reps:
        print(str(each))
        print()
    assert expected == len(reps)
