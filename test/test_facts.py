from thespian.actors import *
import Briareus.BCGen.Parser as Parser
import Briareus.BCGen.Description as D
from Briareus.BCGen.Generator import Generator
from git_example1 import GitExample1


input_spec = '''
{
  "Repos" : [ ("R1", "r1_url"),
              ("R2", "r2_url"),
              ("R3", "r3_url"),
              ("R5", "r5_url"),
              ("R6", "r6_url") ]
, "Branches" : [ "master", "feat1", "dev" ]
, "Variables" : {
      "ghcver" : [ "ghc844", "ghc865", "ghc881" ],
      "c_compiler" : [ "gnucc", "clang", "MSVC" ],
  }
}
'''

def test_input_parser():
    parser = Parser.BISParser()
    inp = parser.parse(input_spec)
    assert expected_inp == inp

def test_example_facts():
    asys = ActorSystem(transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample1, globalName="GetGitInfo")
        # Replication of BCGen.Operations.BCGengenerate()
        parser = Parser.BISParser()
        gen = Generator(actor_system = asys)
        inp = parser.parse(input_spec)
        (rtype, facts) = gen.generate_build_configs(inp, up_to="facts")
        assert rtype == "facts"
        assert expected_facts == sorted(map(str, facts))
    finally:
        asys.shutdown()


expected_inp = D.InputDesc(
    RL = sorted([ D.RepoDesc(repo_name="R1", repo_url="r1_url", project_repo=True),
                  D.RepoDesc(repo_name="R2", repo_url="r2_url"),
                  D.RepoDesc(repo_name="R3", repo_url="r3_url"),
                  D.RepoDesc(repo_name="R5", repo_url="r5_url"),
                  D.RepoDesc(repo_name="R6", repo_url="r6_url"),
    ]),
    BL = sorted([ D.BranchDesc(branch_name="master"),
                  D.BranchDesc(branch_name="feat1"),
                  D.BranchDesc(branch_name="dev"),
    ]),
    VAR = [ D.VariableDesc(variable_name="ghcver",
                           variable_values=["ghc844", "ghc865", "ghc881"]),
            D.VariableDesc(variable_name="c_compiler",
                           variable_values=["gnucc", "clang", "MSVC"]),
    ])


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
project("R1").
repo("R1").
repo("R2").
repo("R3").
repo("R5").
repo("R6").
subrepo("R4").
subrepo("R7").
branch("master").
branch("feat1").
branch("dev").
branch("R1", "master").
branch("R1", "feat1").
branch("R2", "master").
branch("R3", "master").
branch("R5", "master").
branch("R5", "dev").
branch("R6", "master").
branch("R6", "feat1").
branch("R3", "blah").
branch("R5", "blah").
branch("R4", "master").
branch("R4", "feat1").
branch("R7", "master").
branch("R5", "bugfix9").
pullreq("R1", "1", "blah").
pullreq("R4", "8192", "bugfix9").
pullreq("R2", "23", "bugfix9").
submodule("R1", "master", "R2", "r2_master_head").
submodule("R1", "master", "R3", "r3_master_head^3").
submodule("R1", "master", "R4", "r4_master_head^1").
submodule("R1", "feat1", "R2", "r2_master_head^1").
submodule("R1", "feat1", "R3", "r3_master_head").
submodule("R1", "feat1", "R4", "r4_feat1_head^2").
submodule("R1", "blah", "R2", "r2_master_head").
submodule("R1", "blah", "R3", "r3_master_head").
submodule("R1", "blah", "R7", "r7_master_head^4").
varname("ghcver").
varname("c_compiler").
var("ghcver", "ghc844").
var("ghcver", "ghc865").
var("ghcver", "ghc881").
var("c_compiler", "gnucc").
var("c_compiler", "clang").
var("c_compiler", "MSVC").
'''.split('\n')))

# Note: the above does not contain branch("R2", "bugfix9").  This is
# because the optimization in InternalOps previously determined that
# bugfix9 was a pullreq on R2, so it suppressed the query.
