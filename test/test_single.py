# Simple tests for a single repository, which perforce has no submodules.

import json
import pytest
from thespian.actors import *
from git_single import GitTestSingle
from Briareus.Types import BldConfig, BldRepoRev, BranchReq, MainBranch, PR_Grouped
from Briareus.VCS.InternalMessages import *


def test_repo_info(generated_repo_info):
    assert generated_repo_info[1] == expected_repo_info

expected_repo_info = {
    'branches' : set([
        BranchRef(reponame='TheRepo', branchname='feat1', branchref='feat1-ref'),
        BranchRef(reponame='TheRepo', branchname='master', branchref='master-ref'),
    ]),
    'pullreqs': set([
        PRInfo(pr_target_repo='TheRepo', pr_srcrepo_url='frog_repo_url', pr_branch='frog',
               pr_revision='frog_mergeref', pr_ident='91', pr_status=PRSts_Active(),
               pr_title='Croaking frogs', pr_user='frog', pr_email='frog@lilypond.pad'),
        PRInfo(pr_target_repo='TheRepo', pr_srcrepo_url='toad_repo_url', pr_branch='toad',
               pr_revision='toad_mergeref', pr_ident='134', pr_status=PRSts_Active(),
               pr_title='Hoppy toads', pr_user='hoppy', pr_email=''),
    ]),
    'subrepos': set([]),
    'submodules': set([]),
}


def test_example_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))


gitactor = GitTestSingle
input_spec = open('test/inp_single').read()

expected_facts = sorted(filter(None, '''
:- discontiguous project/1.
:- discontiguous project/2.
:- discontiguous repo/2.
:- discontiguous main_branch/2.
:- discontiguous subrepo/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous branch_ref/3.
:- discontiguous pullreq/7.
:- discontiguous varname/2.
:- discontiguous varvalue/4.
project("TheRepo").
project("TheRepo", "TheRepo").
repo("TheRepo", "TheRepo").
default_main_branch("master").
branchreq("TheRepo", "feat1").
branchreq("TheRepo", "dev").
branch("TheRepo", "master").
branch("TheRepo", "feat1").
branch_ref("TheRepo", "master", "master-ref").
branch_ref("TheRepo", "feat1", "feat1-ref").
pullreq("TheRepo", "134", "toad", "toad_mergeref", prsts_active, "hoppy", "").
pullreq("TheRepo", "91", "frog", "frog_mergeref", prsts_active, "frog", "frog@lilypond.pad").
'''.split('\n')))


def skiptest_single_raw_build_config():
    # n.b. neither simpleSystemBase nor multiprocQueueBase support
    # ThespianWatch, so the swipl run takes the full PROLOG_TIMEOUT
    # for both; the multiprocTCPBase or multiprocUDPBase will support
    # ThespianWatch and are therefore much faster.
    import Briareus.BCGen.Operations as BCGen
    import Briareus.Input.Operations as BInput
    import Briareus.BCGen.Generator as Generator
    import Briareus.BuildSys.Hydra as BldSys
    asys = ActorSystem('multiprocTCPBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitTestSingle, globalName="GetGitInfo")
        gen = Generator.Generator(actor_system = asys)
        (rtype, cfgs) = gen.generate_build_configs(
            *BInput.input_desc_and_VCS_info(input_spec,
                                            actor_system=asys,
                                            verbose=True),
            up_to="raw_logic_output")
        # Note that this compares simple strings; the logic evaluation
        # is not stable for ordering and will cause false negatives
        # here.
        assert 'raw_logic_output' == rtype
        print('CFG',cfgs)
        assert expected_raw_build_config == cfgs
    finally:
        asys.shutdown()

expected_raw_build_config = '''[
bldcfg("TheRepo",pullreq,"frog",standard,[bld("TheRepo","frog",brr(3))],[]),
bldcfg("TheRepo",pullreq,"toad",standard,[bld("TheRepo","toad",brr(3))],[]),
bldcfg("TheRepo",regular,"dev",standard,[bld("TheRepo","master",brr(2))],[]),
bldcfg("TheRepo",regular,"feat1",standard,[bld("TheRepo","feat1",brr(1))],[]),
bldcfg("TheRepo",regular,"master",standard,[bld("TheRepo","master",brr(1))],[])
]'''.replace('\n','')


def test_single_internal_count(generated_bldconfigs):
    assert 4 == len(generated_bldconfigs.cfg_build_configs)

def test_single_internal_master(generated_bldconfigs):
    expected = BldConfig("TheRepo", "regular", "master", "standard",
                         MainBranch("TheRepo", "master"),
                         [
                             BldRepoRev("TheRepo", "master", "project_primary"),
                         ],
                         [])
    assert expected in generated_bldconfigs.cfg_build_configs

def test_single_internal_feat1(generated_bldconfigs):
    expected = BldConfig("TheRepo", "regular", "feat1", "standard",
                         BranchReq("TheRepo", "feat1"),
                         [
                             BldRepoRev("TheRepo", "feat1", "project_primary"),
                         ],
                         [])
    assert expected in generated_bldconfigs.cfg_build_configs

def test_single_internal_dev(generated_bldconfigs):
    # The VCS info indicates there is no "dev" branch for TheRepo, so
    # this build configuration should not exist.
    expected = BldConfig("TheRepo", "regular", "dev", "standard",
                         BranchReq("TheRepo", "dev"),
                         [
                             BldRepoRev("TheRepo", "master", "project_primary"),
                         ],
                         [])
    assert expected not in generated_bldconfigs.cfg_build_configs

def test_single_internal_toad(generated_bldconfigs):
    expected = BldConfig("TheRepo", "pullreq", "toad", "standard",
                         PR_Grouped("toad"),
                         [
                             BldRepoRev("TheRepo", "toad", "134"),
                         ],
                         [])
    assert expected in generated_bldconfigs.cfg_build_configs

def test_single_internal_frog(generated_bldconfigs):
    expected = BldConfig("TheRepo", "pullreq", "frog", "standard",
                         PR_Grouped("frog"),
                         [
                             BldRepoRev("TheRepo", "frog", "91"),
                         ],
                         [])
    assert expected in generated_bldconfigs.cfg_build_configs
