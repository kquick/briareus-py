# prototype: cryptol bump_base_compat.  PR is closed in cryptol, but still open in aig (for saw-script).

"""Scenario:

    Proj1/repo1     Proj2/repo2    Proj3/repo3
           |          |  |             |
           +----+-----+  repo2/sub-----+
                |
               RepoA

  Although the configuration is similar to scenario 32, this scenario
  is testing comparability of pr_solo and pr_repogroup PR types.

     * RepoA has a PR on the master branch.

     * repo1 has *two* PR's, both on the master branch.

     * repo2 has a PR on the master branch.  Note that repo2 and
       repo2/sub are in the same actual repository.

     * repo3 has no PRs

  For Proj1, there should be three separate pr_solo PR's built, one
  for each PR in repo1 and one for the PR in RepoA.  These should be
  independently built, since the PR's are assumed to be independent.

  For Proj2, there should be a pr_solo PR build created for the PR in
  RepoA, but a pr_grouped build created for the PR in repo2 that is
  shared with repo2/sub.

  For Proj3, there should be a pr_solo PR build created for the
  repo2/sub PR; note that this was part of the pr_grouped for Proj2,
  but Proj3 does not include the repo2 portion, so it only sees the
  single reference.

  Tests here ensure that the correct pr_type will be generated for
  each project, even when all the projects are considered together and
  that comparing pr_type will appropriately handle comparisons between
  pr_solo and pr_grouped.

"""

from Briareus.Types import (BldConfig, BldRepoRev, BldVariable, BranchReq,
                            PR_Solo, PR_Repogroup, PR_Grouped, PRCfg,
                            MainBranch, PendingStatus,
                            ProjectSummary, StatusReport,
                            PR_Status, PR_Status_Blds)
from Briareus.VCS_API import BranchRef, PRSts_Active, PRInfo, RepoSite, SubModuleInfo
from Briareus.BuildSys import buildcfg_name
import Briareus.hh as hh
import json
import pytest
from datetime import datetime, timedelta

proj1_input_spec = open('test/inp_scenario33_proj1').read()
proj2_input_spec = open('test/inp_scenario33_proj2').read()
proj3_input_spec = open('test/inp_scenario33_proj3').read()


expected_repo_proj1_info = {
    'branches' : set([
        BranchRef(reponame='RepoA', branchname='master', branchref='rA_master_ref'),
        BranchRef(reponame='repo1', branchname='master', branchref='r1_master_ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='RepoA', pr_srcrepo_url='https://github.com/RepoA_prfoo_loc', pr_branch='master',
               pr_revision='rAprFooref', pr_ident='2200', pr_status=PRSts_Active(),
               pr_title='Foo Do 2', pr_user='bar', pr_email='bar@brown.cow'),
        PRInfo(pr_target_repo='repo1', pr_srcrepo_url='https://github.com/Repo1_prbar_loc', pr_branch='master',
               pr_revision='r1prBarref', pr_ident='3333', pr_status=PRSts_Active(),
               pr_title='Bar Do Not', pr_user='bar', pr_email='bar@brown.cow'),
        PRInfo(pr_target_repo='repo1', pr_srcrepo_url='https://github.com/Repo1_prfoo_loc', pr_branch='master',
               pr_revision='r1prFooref', pr_ident='2222', pr_status=PRSts_Active(),
               pr_title='Foo Do', pr_user='bar', pr_email='bar@brown.cow'),
    ]),
    'subrepos': set([
        RepoSite(repo_name='RepoA', repo_url='https://github.com/repoA_loc', main_branch='master', use_submodules=False)
    ]),
    'submodules': set([
        SubModuleInfo(sm_repo_name='repo1', sm_branch='master', sm_pullreq_id=None, sm_sub_name='RepoA', sm_sub_vers='repoA_master_head'),
        SubModuleInfo(sm_repo_name='repo1', sm_branch='master', sm_pullreq_id='3333', sm_sub_name='RepoA', sm_sub_vers='repoA_master_head'),
        SubModuleInfo(sm_repo_name='repo1', sm_branch='master', sm_pullreq_id='2222', sm_sub_name='RepoA', sm_sub_vers='repoA_master_head'),
    ]),
}

expected_repo_proj2_info = {
    'branches' : set([
        BranchRef(reponame='RepoA', branchname='master', branchref='rA_master_ref'),
        BranchRef(reponame='repo2', branchname='dog', branchref='r2_dog_ref'),
        BranchRef(reponame='repo2', branchname='master', branchref='r2_master_ref'),
        BranchRef(reponame='repo2sub', branchname='dog', branchref='r2_dog_ref'),
        BranchRef(reponame='repo2sub', branchname='master', branchref='r2_master_ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='RepoA', pr_srcrepo_url='https://github.com/RepoA_prfoo_loc', pr_branch='master',
               pr_revision='rAprFooref', pr_ident='2200', pr_status=PRSts_Active(),
               pr_title='Foo Do 2', pr_user='bar', pr_email='bar@brown.cow'),
        PRInfo(pr_target_repo='repo2', pr_srcrepo_url='https://github.com/Repo2_prquux_loc', pr_branch='master',
               pr_revision='r2prquuxref', pr_ident='4444', pr_status=PRSts_Active(),
               pr_title='Quux Do', pr_user='zeb', pr_email='zeb@barn.farm'),

        PRInfo(pr_target_repo='repo2sub', pr_srcrepo_url='https://github.com/Repo2_prquux_loc', pr_branch='master',
               pr_revision='r2prquuxref', pr_ident='4444', pr_status=PRSts_Active(),
               pr_title='Quux Do', pr_user='zeb', pr_email='zeb@barn.farm'),
    ]),
    'subrepos': set([
        RepoSite(repo_name='RepoA', repo_url='https://github.com/repoA_loc', main_branch='master', use_submodules=False)
    ]),
    'submodules': set([
        SubModuleInfo(sm_repo_name='repo2', sm_branch='master', sm_pullreq_id=None, sm_sub_name='RepoA', sm_sub_vers='repoA_master_head'),
        SubModuleInfo(sm_repo_name='repo2', sm_branch='master', sm_pullreq_id='4444', sm_sub_name='RepoA', sm_sub_vers='repoA_master_head'),
    ]),
}

expected_repo_proj3_info = {
    'branches' : set([
        BranchRef(reponame='repo3', branchname='master', branchref='r3_master_ref'),
        BranchRef(reponame='repo2sub', branchname='master', branchref='r2_master_ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='repo2sub', pr_srcrepo_url='https://github.com/Repo2_prquux_loc', pr_branch='master',
               pr_revision='r2prquuxref', pr_ident='4444', pr_status=PRSts_Active(),
               pr_title='Quux Do', pr_user='zeb', pr_email='zeb@barn.farm'),
    ]),
    'subrepos': set([
    ]),
    'submodules': set([
    ]),
}


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
:- discontiguous varvalue/4.
project("proj1").
project("proj1", "repo1").
default_main_branch("master").
branch("repo1", "master").
branch_ref("RepoA", "master", "rA_master_ref").
branch_ref("repo1", "master", "r1_master_ref").
repo("proj1", "repo1").
subrepo("repo1", "RepoA").
branch("RepoA", "master").
submodule("repo1", project_primary, "master", "RepoA", "repoA_master_head").
submodule("repo1", "2222", "master", "RepoA", "repoA_master_head").
submodule("repo1", "3333", "master", "RepoA", "repoA_master_head").
pullreq("repo1", "2222", "master", "r1prFooref", prsts_active, "bar", "bar@brown.cow").
pullreq("repo1", "3333", "master", "r1prBarref", prsts_active, "bar", "bar@brown.cow").
pullreq("RepoA", "2200", "master", "rAprFooref", prsts_active, "bar", "bar@brown.cow").
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
:- discontiguous varvalue/4.
project("proj2").
project("proj2", "repo2").
default_main_branch("master").
branch_ref("RepoA", "master", "rA_master_ref").
branch_ref("repo2", "dog", "r2_dog_ref").
branch_ref("repo2", "master", "r2_master_ref").
branch_ref("repo2sub", "dog", "r2_dog_ref").
branch_ref("repo2sub", "master", "r2_master_ref").
branchreq("proj2", "dog").
branch("repo2", "master").
branch("repo2", "dog").
branch("repo2sub", "master").
branch("repo2sub", "dog").
repo("proj2", "repo2").
repo("proj2", "repo2sub").
subrepo("repo2", "RepoA").
branch("RepoA", "master").
submodule("repo2", project_primary, "master", "RepoA", "repoA_master_head").
submodule("repo2", "4444", "master", "RepoA", "repoA_master_head").
pullreq("RepoA", "2200", "master", "rAprFooref", prsts_active, "bar", "bar@brown.cow").
pullreq("repo2", "4444", "master", "r2prquuxref", prsts_active, "zeb", "zeb@barn.farm").
pullreq("repo2sub", "4444", "master", "r2prquuxref", prsts_active, "zeb", "zeb@barn.farm").
'''.split('\n')

proj3_expected_facts = '''
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
:- discontiguous varvalue/4.
project("proj3").
project("proj3", "repo3").
default_main_branch("master").
branch("repo3", "master").
branch("repo2sub", "master").
branch_ref("repo3", "master", "r3_master_ref").
branch_ref("repo2sub", "master", "r2_master_ref").
repo("proj3", "repo3").
repo("proj3", "repo2sub").
pullreq("repo2sub", "4444", "master", "r2prquuxref", prsts_active, "zeb", "zeb@barn.farm").
'''.split('\n')

proj1_top_level = [
        "regular master heads",
        "regular master submodules",
        "pullreq foo heads",
        "pullreq foo submodules",
        "pullreq bar heads",
        "pullreq bar submodules",
        "pullreq fooA RepoA heads",
        "pullreq fooA RepoA submodules",
    ]

proj2_top_level = [
        "regular master heads",
        "regular master submodules",
        "pullreq fooA RepoA heads",
        "pullreq fooA RepoA submodules",
        "pullreq quux heads",
        "pullreq quux submodules",
        "regular dog standard",
    ]

proj3_top_level = [
        "regular master standard",
        "pullreq quux RepoA standard",
    ]


@pytest.fixture(scope="session")
def testing_dir(tmpdir_factory):
    return tmpdir_factory.mktemp("scenario33")

@pytest.fixture(scope="module")
def inp_configs(testing_dir):
    outfile_p1 = testing_dir.join("p1.hhc")
    outfile_p2 = testing_dir.join("p2.hhc")
    outfile_p3 = testing_dir.join("p3.hhc")
    return [
        (expected_repo_proj1_info, outfile_p1,
         hh.InpConfig(hhd='test/inp_scenario33_proj1',
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_p1),
        ),
        (expected_repo_proj2_info, outfile_p2,
         hh.InpConfig(hhd='test/inp_scenario33_proj2',
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_p2),
        ),
        (expected_repo_proj3_info, outfile_p3,
         hh.InpConfig(hhd='test/inp_scenario33_proj3',
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_p3),
        ),
    ]


def test_input_facts(generated_inp_config_facts):
    assert sorted(
        filter(None,
               proj1_expected_facts +
               proj2_expected_facts +
               proj3_expected_facts
        )) == sorted(map(str, generated_inp_config_facts))

def test_proj1_bldcfg_count(generated_inp_config_bldconfigs):
    # Uncomment this to see all hydra jobnames
    # for each in generated_inp_config_bldconfigs.result_sets:
    #     print([R for R in each.inp_desc.RL if R.project_repo][0].repo_name)
    #     for cfgnum, eachcfg in enumerate(each.build_cfgs.cfg_build_configs):
    #         print('',cfgnum,eachcfg) #buildcfg_name(eachcfg))
    #     print('')
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

def test_proj3_bldcfg_count(generated_inp_config_bldconfigs):
    # Uncomment this to see all hydra jobnames
    # for each in generated_inp_config_bldconfigs.result_sets:
    #     print([R for R in each.inp_desc.RL if R.project_repo][0].repo_name)
    #     for cfgnum, eachcfg in enumerate(each.build_cfgs.cfg_build_configs):
    #         print('',cfgnum,buildcfg_name(eachcfg))
    #     print('')
    results = [ Res for Res in generated_inp_config_bldconfigs.result_sets
                if any([R for R in Res.inp_desc.RL
                        if R.project_repo and R.repo_name == 'repo3' ])][0]
    assert len(proj3_top_level) == len(results.build_cfgs.cfg_build_configs)


def test_proj1_regular_master(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj1':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A regular non-PR build exists.
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj1',
                                 branchtype='regular',
                                 branchname='master',
                                 strategy=strategy,
                                 description=MainBranch(reponame='repo1',
                                                        branchname='master'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover={'HEADs':'master',
                                                         'submodules':'repoA_master_head',
                                                         }[strategy],
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo1',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                 ],
                                 bldvars=[]) in bldcfgs

def test_proj1_first_pr_master_repo1(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj1':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_solo build exists for the first PR in repo1.  The
            # master and submodules are identical because the PR in
            # repo1 does not change the submodules
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj1',
                                 branchtype='pullreq',
                                 branchname='master',
                                 strategy=strategy,
                                 description=PR_Solo(projectname='proj1',
                                                     reponame='repo1',
                                                     pullreq_id='2222'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover={'HEADs':'master',
                                                         'submodules':'repoA_master_head',
                                                         }[strategy],
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo1',
                                                repover='master',
                                                pullreq_id='2222'),
                                 ],
                                 bldvars=[]) in bldcfgs

def test_proj1_second_pr_master_repo1(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj1':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_solo build exists for the second PR in repo1
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj1',
                                 branchtype='pullreq',
                                 branchname='master',
                                 strategy=strategy,
                                 description=PR_Solo(projectname='proj1',
                                                     reponame='repo1',
                                                     pullreq_id='3333'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover={'HEADs':'master',
                                                         'submodules':'repoA_master_head',
                                                         }[strategy],
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo1',
                                                repover='master',
                                                pullreq_id='3333'),
                                 ],
                                 bldvars=[]) in bldcfgs

def test_proj1_pr_master_repoA(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj1':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_solo build exists for the PR in repoA.  master and
            # submodules are identical because the same revision in
            # repo1 is used.
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj1',
                                 branchtype='pullreq',
                                 branchname='master',
                                 strategy=strategy,
                                 description=PR_Solo(projectname='proj1',
                                                     reponame='RepoA',
                                                     pullreq_id='2200'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover='master',
                                                pullreq_id='2200'),
                                     BldRepoRev(reponame='repo1',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                 ],
                                 bldvars=[]) in bldcfgs


def test_proj2_regular_master(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj2':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A regular non-PR build exists.
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj2',
                                 branchtype='regular',
                                 branchname='master',
                                 strategy=strategy,
                                 description=MainBranch(reponame='repo2',
                                                        branchname='master'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover={'HEADs':'master',
                                                         'submodules':'repoA_master_head',
                                                         }[strategy],
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo2',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo2sub',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                 ],
                                 bldvars=[]) in bldcfgs


def test_proj2_pr_master_repoA(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj2':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_solo build exists for the PR in repoA.  master and
            # submodules are identical because the same revision in
            # repo2 is used.
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj2',
                                 branchtype='pullreq',
                                 branchname='master',
                                 strategy=strategy,
                                 description=PR_Solo(projectname='proj2',
                                                     reponame='RepoA',
                                                     pullreq_id='2200'),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover='master',
                                                pullreq_id='2200'),
                                     BldRepoRev(reponame='repo2',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo2sub',
                                                repover='master',
                                                pullreq_id='project_primary'),
                                 ],
                                 bldvars=[]) in bldcfgs


def test_proj2_pr_master_repo2sub(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj2':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_repogroup build exists for the PR in repo2. The
            # master and submodules are identical because the same
            # revision in repo2 is used.
            for strategy in ( 'HEADs', 'submodules' ):
                assert BldConfig(projectname='proj2',
                                 branchtype='pullreq',
                                 branchname='master',
                                 strategy=strategy,
                                 description=PR_Repogroup(projectname='proj2',
                                                          pullreq_id='4444',
                                                          reponames=['repo2',
                                                                     'repo2sub',
                                                          ],),
                                 blds=[
                                     BldRepoRev(reponame='RepoA',
                                                repover={'HEADs':'master',
                                                         'submodules':'repoA_master_head',
                                                         }[strategy],
                                                pullreq_id='project_primary'),
                                     BldRepoRev(reponame='repo2',
                                                repover='master',
                                                pullreq_id='4444'),
                                     BldRepoRev(reponame='repo2sub',
                                                repover='master',
                                                pullreq_id='4444'),
                                 ],
                                 bldvars=[]) in bldcfgs


def test_proj3_regular_master(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj3':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A regular non-PR build exists.
            assert BldConfig(projectname='proj3',
                             branchtype='regular',
                             branchname='master',
                             strategy='standard',
                             description=MainBranch(reponame='repo3',
                                                    branchname='master'),
                             blds=[
                                 BldRepoRev(reponame='repo3',
                                            repover='master',
                                            pullreq_id='project_primary'),
                                 BldRepoRev(reponame='repo2sub',
                                            repover='master',
                                            pullreq_id='project_primary'),
                             ],
                             bldvars=[]) in bldcfgs


def test_proj3_pr_master_repo2sub(generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        if each.inp_desc.PNAME == 'proj3':
            bldcfgs = each.build_cfgs.cfg_build_configs
            # A PR_solo build exists for the PR in repo2sub. The
            # master and submodules are identical because the same
            # revision in repo2 is used.
            assert BldConfig(projectname='proj3',
                             branchtype='pullreq',
                             branchname='master',
                             strategy='standard',
                             description=PR_Solo(projectname='proj3',
                                                 reponame='repo2sub',
                                                 pullreq_id='4444'),
                             blds=[
                                BldRepoRev(reponame='repo3',
                                           repover='master',
                                           pullreq_id='project_primary'),
                                 BldRepoRev(reponame='repo2sub',
                                            repover='master',
                                            pullreq_id='4444'),
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
               "PR2222-master.HEADs",
               "PR2222-master.submodules",
               "PR3333-master.HEADs",
               "PR3333-master.submodules",
               "PR2200-master.HEADs",
               "PR2200-master.submodules",
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
               "PR2200-master.HEADs",
               "PR2200-master.submodules",
               "PR4444-master.HEADs",
               "PR4444-master.submodules",
               "dog.standard",
    ]
]

proj3_build_results = [
    { "name": n,
      "nrtotal" : 2,
      "nrsucceeded": 1,
      "nrfailed": 0,
      "nrscheduled": 1,
      "haserrormsg": False,
      "fetcherrormsg": '',
    }
    for n in [ "master.standard",
               "PR4444-master.standard",
    ]
]

proj1_prior = []
proj2_prior = []
proj3_prior = []

@pytest.fixture
def builder_report(testing_dir, generated_inp_config_bldconfigs):
    for each in generated_inp_config_bldconfigs.result_sets:
        each.builder._build_results = "project", {
            "repo1": proj1_build_results,
            "repo2": proj2_build_results,
            "repo3": proj3_build_results,
        }[[R.repo_name for R in each.inp_desc.RL if R.project_repo][0]]
    return generate_report(testing_dir,
                           generated_inp_config_bldconfigs,
                           proj1_prior + proj2_prior + proj3_prior)

def generate_report(testdir, inp_config_bldconfigs, prior, reporting_logic_defs=''):
    params = hh.Params(verbose=True, up_to=None,
                       report_file=testdir.join("scenario33.hhr"))
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

    assert ProjectSummary(project_name='proj1+proj2+proj3',
                          bldcfg_count=(len(proj1_top_level) +
                                        len(proj2_top_level) +
                                        len(proj3_top_level)
                                        ),
                          subrepo_count=2,
                          pullreq_count=7) in reps


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

    # proj1: 3 pr_solo * 2 [HEADS, submodules]
    # proj2: 1 pr_repogroup and 1 pr_solo * 2 [HEADS, submodules]
    # proj3: 1 pr_solo * 1 [standard]
    assert (3*2) + (2*2) + 1 == len([r for r in reps if isinstance(r, PR_Status)])

def test_report_prstatus_proj1_present(builder_report):
    reps = builder_report.report
    for strategy in ( 'HEADs', 'submodules'):
        assert PR_Status(prtype=PR_Grouped('foo'),
                         branch='foo',
                         project='proj1',
                         strategy=strategy,
                         prcfg=[PRCfg(reponame='repo1',
                                      pr_id='2222',
                                      branch='foo',
                                      revision='r1prFooref',
                                      user='bar',
                                      email='bar@brows.cow'),
                         ],
                         prstatus_blds=PR_Status_Blds(
                             passing=['2222.HEADs',
                                      '2222.submodules',
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
                                      pr_id='2222',
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

    assert (len(proj1_top_level) +
            len(proj2_top_level) +
            len(proj3_top_level)
    ) == len([r for r in reps
              if isinstance(r, PendingStatus)])
