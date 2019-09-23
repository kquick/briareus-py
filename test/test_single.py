# Simple tests for a single repository, which perforce has no submodules.

from thespian.actors import *
import json
import pytest
import Briareus.BCGen.Operations as BCGen
import Briareus.BCGen.BuildConfigs as CFG
import Briareus.Input.Parser as Parser
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from Briareus.VCS.InternalMessages import *


input_spec = '''
{
  "Repos" : [ ("TheRepo", "the_repo_url") ]
, "Branches" : [ "master", "feat1", "dev" ]
}
'''

def test_single_facts():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitTestSingle, globalName="GetGitInfo")
        # Replication of BCGen.Operations.BCGengenerate()
        parser = Parser.BISParser()
        gen = Generator.Generator(actor_system = asys)
        inp = parser.parse(input_spec)
        (rtype, facts) = gen.generate_build_configs(inp, up_to="facts")
        assert rtype == "facts"
        assert expected_facts == sorted(map(str, facts))
    finally:
        asys.shutdown()


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


expected_facts = sorted(filter(None, '''
:- dynamic project/1.
:- dynamic repo/1.
:- dynamic subrepo/1.
:- dynamic submodule/4.
:- dynamic branch/1.
:- dynamic branch/2.
:- dynamic pullreq/3.
:- dynamic varname/1.
:- dynamic var/2.
project("TheRepo").
repo("TheRepo").
branch("master").
branch("feat1").
branch("dev").
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
    asys = ActorSystem('multiprocTCPBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitTestSingle, globalName="GetGitInfo")
        parser = Parser.BISParser()
        gen = Generator.Generator(actor_system = asys)
        (rtype, cfgs) = gen.generate_build_configs(parser.parse(input_spec),
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
bldcfg(pullreq,"frog",main,[bld("TheRepo","frog",brr(3))],[]),
bldcfg(pullreq,"toad",main,[bld("TheRepo","toad",brr(3))],[]),
bldcfg(regular,"dev",main,[bld("TheRepo","master",brr(2))],[]),
bldcfg(regular,"feat1",main,[bld("TheRepo","feat1",brr(1))],[]),
bldcfg(regular,"master",main,[bld("TheRepo","master",brr(1))],[])
]'''.replace('\n','')

@pytest.fixture(scope="module")
def single_internal_bldconfigs():
    asys = ActorSystem('multiprocTCPBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitTestSingle, globalName="GetGitInfo")
        parser = Parser.BISParser(verbose=True)
        gen = Generator.Generator(actor_system = asys, verbose=True)
        (_rtype, cfgs) = gen.generate_build_configs(parser.parse(input_spec))
        yield cfgs
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

def test_single_internal_count(single_internal_bldconfigs):
    assert 5 == len(single_internal_bldconfigs.cfg_build_configs)

def test_single_internal_master(single_internal_bldconfigs):
    expected = CFG.BldConfig("regular", "master", "main", [CFG.BldRepoRev("TheRepo","master")], [])
    assert expected in single_internal_bldconfigs.cfg_build_configs

def test_single_internal_feat1(single_internal_bldconfigs):
    expected = CFG.BldConfig("regular", "feat1", "main", [CFG.BldRepoRev("TheRepo","feat1")], [])
    assert expected in single_internal_bldconfigs.cfg_build_configs

def test_single_internal_dev(single_internal_bldconfigs):
    expected = CFG.BldConfig("regular", "dev", "main", [CFG.BldRepoRev("TheRepo","master")], [])
    assert expected in single_internal_bldconfigs.cfg_build_configs

def test_single_internal_toad(single_internal_bldconfigs):
    expected = CFG.BldConfig("pullreq", "toad", "main", [CFG.BldRepoRev("TheRepo","toad")], [])
    assert expected in single_internal_bldconfigs.cfg_build_configs

def test_single_internal_frog(single_internal_bldconfigs):
    expected = CFG.BldConfig("pullreq", "frog", "main", [CFG.BldRepoRev("TheRepo","frog")], [])
    assert expected in single_internal_bldconfigs.cfg_build_configs
