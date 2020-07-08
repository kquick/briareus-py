from Briareus.Types import BldConfig, BldRepoRev, BldVariable, BranchReq, PR_Grouped, MainBranch
from Briareus.VCS_API import BranchRef, PRSts_Active, PRSts_Merged, PRInfo, RepoSite, SubModuleInfo
import json
import pytest

input_spec = open('test/inp_prsubmodule').read()

expected_repo_info = {
    'branches' : set([
        BranchRef(reponame='SubRepo1', branchname='master', branchref='SubRepo1-master-ref'),
        BranchRef(reponame='TopRepo', branchname='master', branchref='TopRepo-master-ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='SubRepo1', pr_srcrepo_url='https://github.com/subrepo1_pr312_loc', pr_branch='subfix',
               pr_revision='sr1pr312sf3', pr_ident='312', pr_status=PRSts_Active(),
               pr_title='better', pr_user='dev', pr_email='dev@soft.ware'),
    ]),
    'subrepos': set([
        RepoSite(repo_name='SubRepo1', repo_url='https://github.com/subrepo_loc', main_branch='master', use_submodules=False),
    ]),
    'submodules': set([
        SubModuleInfo(sm_repo_name='TopRepo', sm_branch='master', sm_pullreq_id=None, sm_sub_name='SubRepo1', sm_sub_vers='subrepo_master_head'),
    ]),
}

expected_facts = sorted(filter(None, '''
:- discontiguous project/1.
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous subrepo/2.
:- discontiguous main_branch/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous branch_ref/3.
:- discontiguous pullreq/7.
:- discontiguous varname/2.
:- discontiguous varvalue/4.
project("prsubtest").
project("prsubtest", "TopRepo").
default_main_branch("master").
branch("TopRepo", "master").
branch_ref("TopRepo", "master", "TopRepo-master-ref").
repo("prsubtest", "TopRepo").
subrepo("TopRepo", "SubRepo1").
branch("SubRepo1", "master").
branch_ref("SubRepo1", "master", "SubRepo1-master-ref").
submodule("TopRepo", project_primary, "master", "SubRepo1", "subrepo_master_head").
pullreq("SubRepo1", "312", "subfix", "sr1pr312sf3", prsts_active, "dev", "dev@soft.ware").
'''.split('\n')))


top_level = [
    "regular master heads",
    "regular master submodules",
    "pullreq subfix heads",
    "pullreq subfix submodules",
]


def test_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))


def test_bldcfg_count(generated_bldconfigs):
    print('### bldcfgs:')
    for each in generated_bldconfigs.cfg_build_configs:
        print(each.projectname, each.branchtype, each.branchname, each.strategy)
    assert len(top_level) == len(generated_bldconfigs.cfg_build_configs)


def test_bldcfg_master(generated_bldconfigs):
    for each in [ BldConfig(projectname="prsubtest",
                            branchtype="regular",
                            branchname="master",
                            strategy=S,
                            description=MainBranch("TopRepo", "master"),
                            blds=[BldRepoRev("TopRepo", "master", "project_primary"),
                                  BldRepoRev("SubRepo1",
                                             "master" if S == "HEADs" else "subrepo_master_head",
                                             "project_primary"),
                            ],
                            bldvars=[])
                  for S in [ "HEADs", "submodules"] ]:
        assert each in generated_bldconfigs.cfg_build_configs

def test_bldcfg_subfix(generated_bldconfigs):
    for each in generated_bldconfigs.cfg_build_configs:
        print(each)
        print('')
    for each in [ BldConfig(projectname="prsubtest",
                            branchtype="pullreq",
                            branchname="subfix",
                            strategy=S,
                            description=PR_Grouped("subfix"),
                            # note: the build configuration is
                            # identical for HEADs and submodules for
                            # this project.
                            blds=[BldRepoRev("TopRepo", "master", "project_primary"),
                                  BldRepoRev('SubRepo1', 'subfix', '312'),
                            ],
                            bldvars=[])
                  for S in [ "HEADs", "submodules" ] ]:
        assert each in generated_bldconfigs.cfg_build_configs
