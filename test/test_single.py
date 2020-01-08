# Simple tests for a single repository, which perforce has no submodules.

import json
import pytest
from thespian.actors import *
from Briareus.Types import BldConfig, BldRepoRev, BranchReq, MainBranch, PR_Grouped
from Briareus.VCS.InternalMessages import *


input_spec = '''
{
  "Repos" : [ ("TheRepo", "the_repo_url") ]
, "Branches" : [ "feat1", "dev" ]
}
'''
# n.b. "master" is not listed in Branches, but a configuration for
# master will be generated because it is the main branch for TheRepo.

def test_example_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))


class GitTestSingle(ActorTypeDispatcher):
    def __init__(self, *args, **kw):
        super(GitTestSingle, self).__init__(*args, **kw)

    def receiveMsg_DeclareRepo(self, msg, sender):
        self.send(sender, RepoDeclared(msg.reponame))

    def receiveMsg_GetPullReqs(self, msg, sender):
        self.send(sender, PullReqsData(msg.reponame,
                                       [PullReqInfo(134, 'Hoppy toads', 'toad_repo_url', 'toad', 'toad_mergeref'),
                                        PullReqInfo(91, 'Croaking frogs', 'frog_repo_url', 'frog', 'frog_mergeref')]))

    def receiveMsg_HasBranch(self, msg, sender):
        branch = msg.branch_name
        self.send(sender, BranchPresent(msg.reponame, branch,
                                        branch in ['master', 'feat1']))
        # Note that toad and frog are not in the branch list because
        # those exist on the remote toad_repo_url and frog_repo_url,
        # not on TheRepo.

    def receiveMsg_GitmodulesData(self, msg, sender):
        branch = msg.branch_name
        self.send(sender, GitmodulesRepoVers(msg.reponame, branch, []))

    def receiveMsg_Repo_AltLoc_ReqMsg(self, msg, sender):
        assert isinstance(msg.altloc_reqmsg, GitmodulesData)
        self.receiveMsg_GitmodulesData(msg.altloc_reqmsg, sender)

gitactor = GitTestSingle


expected_facts = sorted(filter(None, '''
:- discontiguous project/1.
:- discontiguous repo/2.
:- discontiguous main_branch/2.
:- discontiguous subrepo/2.
:- discontiguous submodule/5.
:- discontiguous branchreq/2.
:- discontiguous branch/2.
:- discontiguous pullreq/3.
:- discontiguous varname/2.
:- discontiguous varvalue/3.
project("TheRepo").
repo("TheRepo", "TheRepo").
default_main_branch("master").
branchreq("TheRepo", "feat1").
branchreq("TheRepo", "dev").
branch("TheRepo", "master").
branch("TheRepo", "feat1").
pullreq("TheRepo", "134", "toad").
pullreq("TheRepo", "91", "frog").
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
