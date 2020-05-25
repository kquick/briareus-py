# prototype: cryptol bump_base_compat.  PR is closed in cryptol, but still open in aig (for saw-script).

"""Scenario:

    Proj1/repo1     Proj2/repo2
           |          |
           +----+-----+
                |
               RepoA

  1. PR "foo" created in RepoA
     * Result is a PR-foo build for Proj1 and Proj2

  2. PR "foo" created in repo1
     * Proj1 now builds with foo of repo1 and RepoA
     * Proj2 builds with repo2 master, repoA foo

  3. PR "foo" in RepoA is merged to master

     * Proj2 now has only a master build

     * Proj1 still builds "foo in repo1 and RepoA
       - This should build against master of RepoA [issue#1]

     - Reporting thinks there should be a PR-foo build in Proj2,
       because the Proj1-driven fact gathering identifies that RepoA
       has a foo branch, so in the combined analysis report, it thinks
       there should be a build config for foo in Proj2 (but there
       isn't because the build config is determined prior to project
       merging), and there is no build status for foo in Proj2, so it
       always reports it as new_pending [issue#2].

  Issue#1: want Proj1 to build master of RepoA and not Proj1 because
  we should not use potentially ancient branches if a PR name happens
  to get re-used.

  Issue#2: no pending report for non-active build; don't cross-wire
  between projects.

  The solution to both of the above is that for any repo where a
  branch is probed, it should check if there is a PR associated with
  that branch, and if there is and the PR is closed or merged then the
  branch should be ignored.

"""

from Briareus.Types import (BldConfig, BldRepoRev, BldVariable, BranchReq,
                            PR_Grouped, MainBranch, PRCfg, PendingStatus,
                            ProjectSummary, StatusReport,
                            PR_Status, PR_Status_Blds)
from Briareus.BuildSys import buildcfg_name
from git_scenario32 import GitScenario32
import Briareus.hh as hh
import json
import pytest
from datetime import datetime, timedelta


proj1_input_spec = '''
{
    "Name" : "proj1",
    "Repos" : [ ("repo1", "repo1_loc"),
              ],
    "Reporting": {
      "logic": """
         enable(forge_status, _, _).
      """,
    },
}
'''

proj2_input_spec = '''
{
    "Name" : "proj2",
    "Repos" : [ ("repo2", "repo2_loc"),
              ],
    "Branches" : [ 'dog' ],
    "Reporting": {
      "logic": """
         enable(forge_status, _, _).
      """,
    },
}
'''

proj1_expected_facts = '''
:- discontiguous project/1.
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous subrepo/2.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch_ref/3.
:- discontiguous branch/2.
:- discontiguous pullreq/7.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("proj1").
project("proj1", "repo1").
default_main_branch("master").
branch("repo1", "master").
branch("repo1", "foo").
branch_ref("RepoA", "master", "rA_master_ref").
branch_ref("repo1", "foo", "r1_foo_ref").
branch_ref("repo1", "master", "r1_master_ref").
repo("proj1", "repo1").
subrepo("repo1", "RepoA").
branch("RepoA", "master").
submodule("repo1", project_primary, "master", "RepoA", "repoA_master_head").
submodule("repo1", "pr-foo", "foo", "RepoA", "repoA_foo_head").
pullreq("repo1", "pr-foo", "foo", "r1prFooref", prsts_active, "bar", "bar@brown.cow").
pullreq("RepoA", "pr-foo", "foo", "rAprFooref", prsts_merged, "bar", "bar@brown.cow").
'''.split('\n')


proj2_expected_facts = '''
:- discontiguous project/1.
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous subrepo/2.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branch_ref/3.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous pullreq/7.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("proj2").
project("proj2", "repo2").
default_main_branch("master").
branch_ref("RepoA", "master", "rA_master_ref").
branch_ref("repo2", "dog", "r2_dog_ref").
branch_ref("repo2", "master", "r2_master_ref").
branchreq("proj2", "dog").
branch("repo2", "master").
branch("repo2", "dog").
repo("proj2", "repo2").
subrepo("repo2", "RepoA").
branch("RepoA", "master").
submodule("repo2", project_primary, "master", "RepoA", "repoA_master_head").
pullreq("RepoA", "pr-foo", "foo", "rAprFooref", prsts_merged, "bar", "bar@brown.cow").
'''.split('\n')


proj1_top_level = [
        "regular master heads",
        "regular master submodules",
        "pullreq foo heads",
        "pullreq foo submodules",
    ]

proj2_top_level = [
        "regular master heads",
        "regular master submodules",
        "regular dog standard",
    ]


@pytest.fixture(scope="session")
def testing_dir(tmpdir_factory):
    return tmpdir_factory.mktemp("scenario32")

@pytest.fixture(scope="session")
def proj1_hhd(testing_dir):
    hhd = testing_dir.join("p1.hdd")
    hhd.write(proj1_input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="session")
def proj2_hhd(testing_dir):
    hhd = testing_dir.join("p2.hdd")
    hhd.write(proj2_input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="module")
def inp_configs(testing_dir, proj1_hhd, proj2_hhd):
    outfile_p1 = testing_dir.join("p1.hhc")
    outfile_p2 = testing_dir.join("p2.hhc")
    return [
        (GitScenario32, outfile_p1, hh.InpConfig(hhd=proj1_hhd,
                                                 builder_type="hydra",
                                                 # builder_conf="
                                                 output_file=outfile_p1,
        ),
        ),
        (GitScenario32, outfile_p2, hh.InpConfig(hhd=proj2_hhd,
                                                 builder_type="hydra",
                                                 # builder_conf="
                                                 output_file=outfile_p2,
        ),
        ),
    ]


def test_input_facts(generated_inp_config_facts):
    assert sorted(
        filter(None,
               proj1_expected_facts +
               proj2_expected_facts)) == sorted(map(str, generated_inp_config_facts))

def test_proj1_bldcfg_count(generated_inp_config_bldconfigs):
    # Uncomment this to see all hydra jobnames
    for each in generated_inp_config_bldconfigs.result_sets:
        print([R for R in each.inp_desc.RL if R.project_repo][0].repo_name)
        for cfgnum, eachcfg in enumerate(each.build_cfgs.cfg_build_configs):
            print('',cfgnum,eachcfg) #buildcfg_name(eachcfg))
        print('')
    results = [ Res for Res in generated_inp_config_bldconfigs.result_sets
                if any([R for R in Res.inp_desc.RL
                        if R.project_repo and R.repo_name == 'repo1' ])][0]
    assert len(proj1_top_level) == len(results.build_cfgs.cfg_build_configs)

def test_proj2_bldcfg_count(generated_inp_config_bldconfigs):
    # Uncomment this to see all hydra jobnames
    # for each in generated_inp_config_bldconfigs.result_sets:
    #     print([R for R in each.inp_desc.RL if R.project_repo][0].repo_name)
    #     for cfgnum, eachcfg in enumerate(each.build_cfgs.cfg_build_configs):
    #         print('',cfgnum,buildcfg_name(eachcfg))
    #     print('')
    results = [ Res for Res in generated_inp_config_bldconfigs.result_sets
                if any([R for R in Res.inp_desc.RL
                        if R.project_repo and R.repo_name == 'repo2' ])][0]
    assert len(proj2_top_level) == len(results.build_cfgs.cfg_build_configs)


def test_issue1_proj1_pr_master_repoA(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj1':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # The HEADs build should build against RepoA master
            # because the foo PR has been merged in RepoA
            assert BldConfig(projectname='proj1',
                             branchtype='pullreq',
                             branchname='foo',
                             strategy='HEADs',
                             description=PR_Grouped(branchname='foo'),
                             blds=[
                                 BldRepoRev(reponame='RepoA',
                                            repover='master',
                                            pullreq_id='project_primary'),
                                 BldRepoRev(reponame='repo1',
                                            repover='foo',
                                            pullreq_id='pr-foo'),
                                 ],
                             bldvars=[]) in bldcfgs
            # The submodules build should respect the gitmodules
            # though, so it still refers to the foo branch in repoA
            assert BldConfig(projectname='proj1',
                             branchtype='pullreq',
                             branchname='foo',
                             strategy='submodules',
                             description=PR_Grouped(branchname='foo'),
                             blds=[
                                 BldRepoRev(reponame='RepoA',
                                            repover='repoA_foo_head',
                                            pullreq_id='project_primary'),
                                 BldRepoRev(reponame='repo1',
                                            repover='foo',
                                            pullreq_id='pr-foo'),
                                 ],
                             bldvars=[]) in bldcfgs

# ----------------------------------------

proj1_build_results = [
    { "name": n,
      "nrtotal" : 2,
      "nrsucceeded": 1,
      "nrfailed": 0,
      "nrscheduled": 1,
      "haserrormsg": False,
      "fetcherrormsg": '',
    }
    for n in [ "master.HEADs",
               "master.submodules",
               "PR-foo.HEADs",
               "PR-foo.submodules",
    ]
]

proj2_build_results = [
    { "name": n,
      "nrtotal" : 2,
      "nrsucceeded": 1,
      "nrfailed": 0,
      "nrscheduled": 1,
      "haserrormsg": False,
      "fetcherrormsg": '',
    }
    for n in [ "master.HEADs",
               "master.submodules",
               "dog.standard",
    ]
]

proj1_prior = []
proj2_prior = []

@pytest.fixture
def builder_report(testing_dir, generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        each.builder._build_results = "project", {
            "repo1": proj1_build_results,
            "repo2": proj2_build_results,
        }[[R.repo_name for R in each.inp_desc.RL if R.project_repo][0]]
    return generate_report(testing_dir, generated_inp_config_bldconfigs, proj1_prior + proj2_prior)

def generate_report(testdir, inp_config_bldconfigs, prior, reporting_logic_defs=''):
    params = hh.Params(verbose=True, up_to=None,
                       report_file=testdir.join("scenario32.hhr"))
    starttime = datetime.now()
    rep = hh.run_hh_report(params, inp_config_bldconfigs, prior,
                           reporting_logic_defs=reporting_logic_defs)
    endtime = datetime.now()
    # This should be a proper test: checks the amount of time to run run the logic process.
    assert endtime - starttime < timedelta(seconds=2, milliseconds=500)  # avg 1.02s
    return rep

def test_report_summary(builder_report):
    reps = builder_report.report

    for each in reps:
        print('')
        print(each)

    assert ProjectSummary(project_name='proj1+proj2',
                          bldcfg_count=len(proj1_top_level) + len(proj2_top_level),
                          subrepo_count=2, pullreq_count=1) in reps


# def test_report_statusreport_count(builder_report):
#     reps = builder_report.report

#     for each in reps:
#         print('')
#         print(each)

#     assert (len(proj1_top_level) +
#             len(proj2_top_level)) == len([r for r in reps if isinstance(r, PendingStatus)])


def test_report_prstatus_count(builder_report):
    reps = builder_report.report

    for each in reps:
        print('')
        print(each)
    print('****')
    # print(len(builder_report.result_sets))
    # print(builder_report.result_sets[0].build_cfgs)
    for each in builder_report.result_sets:
        print('....')
        for cfg in each.build_cfgs.cfg_build_configs:
            print(cfg)
            print('')

    # Should be a PR_Status showing passing for proj1 (for each
    # strategy: HEADs, submodules), since there's still a pr-foo in
    # that repo.  Should NOT be a PR_Status for proj2.
    assert 2 == len([r for r in reps if isinstance(r, PR_Status)])

def test_report_prstatus_proj1_present(builder_report):
    reps = builder_report.report
    for strategy in ( 'HEADs', 'submodules'):
        assert PR_Status(prtype=PR_Grouped('foo'),
                         branch='foo',
                         project='proj1',
                         strategy=strategy,
                         prcfg=[PRCfg(reponame='repo1',
                                      pr_id='pr-foo',
                                      branch='foo',
                                      revision='r1prFooref',
                                      user='bar',
                                      email='bar@brows.cow'),
                         ],
                         prstatus_blds=PR_Status_Blds(
                             passing=['PR-foo.HEADs',
                                      'PR-foo.submodules',
                             ],
                             failing=[],
                             pending=[],
                             unstarted=0)) not in reps


def test_issue2_report_prstatus_proj2_not_present(builder_report):
    reps = builder_report.report
    for strategy in ( 'HEADs', 'submodules'):
        assert PR_Status(prtype=PR_Grouped('foo'),
                         branch='foo',
                         project='proj2',
                         strategy=strategy,
                         prcfg=[PRCfg(reponame='repo2',
                                      pr_id='pr-foo',
                                      branch='foo',
                                      revision='somehash',
                                      user='bar',
                                      email='bar@brows.cow'),
                         ],
                         prstatus_blds=PR_Status_Blds(
                             passing=[],
                             failing=[],
                             pending=[],
                             unstarted=0)) not in reps

def test_report_pendingstatus_count(builder_report):
    reps = builder_report.report

    for each in reps:
        print('')
        print(each)
    print('****')

    # Should be a PR_Status showing passing for proj1, since there's
    # still a pr-foo in that repo.  Should NOT be a PR_Status for proj2.
    assert (len(proj1_top_level) +
            len(proj2_top_level)) == len([r for r in reps
                                          if isinstance(r, PendingStatus)])
