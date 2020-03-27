import Briareus.Input.Parser as Parser
import Briareus.Input.Description as D
from git_example1 import GitExample1
from test_example import input_spec


def test_input_parser():
    parser = Parser.BISParser()
    inp = parser.parse(input_spec)
    assert expected_inp == inp

gitactor = GitExample1

def test_example_facts(generated_facts):
    assert expected_facts == list(map(str, generated_facts))


expected_inp = D.InputDesc(
    PNAME="Project #1",
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
                           variable_values=["gnucc", "clang"]),
    ],
    REP = {'logic': """
project_owner("Project #1", "george@_company.com").

project_owner("R3", "john@not_a_company.com").

enable(email, "fred@nocompany.com", notify(_, "Project #1", _)).
enable(email, "eddy@nocompany.com", notify(_, "Project #1", _)).
enable(email, "sam@not_a_company.com", notify(_, "Project #1", _)).
enable(email, "john@_company.com", notify(_, "Project #1", _)).
enable(email, "anne@nocompany.com", notify(master_submodules_broken, "Project #1", _)).
enable(email, "betty@nocompany.com", notify(variable_failing, "Project #1", _)).

enable(forge_status, "Project #1", _).
      """
    }
    )


expected_facts = sorted(filter(None, '''
:- discontiguous project/1.
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
project("Project #1").
project("Project #1", "R1").
repo("Project #1", "R1").
default_main_branch("master").
repo("Project #1", "R2").
repo("Project #1", "R3").
repo("Project #1", "R5").
repo("Project #1", "R6").
subrepo("R1", "R2").
subrepo("R1", "R3").
subrepo("R1", "R4").
subrepo("R1", "R7").
branchreq("Project #1", "master").
branchreq("Project #1", "feat1").
branchreq("Project #1", "dev").
branch("R1", "master").
branch("R1", "feat1").
branch("R2", "bugfix9").
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
pullreq("R1", "1", "blah", prsts_active, "nick", "nick@bad.seeds").
pullreq("R4", "8192", "bugfix9", prsts_new, "ozzie", "ozzie@crazy.train").
pullreq("R2", "23", "bugfix9", prsts_active, "banana", "").
pullreq("R3", "11", "blah", prsts_active, "nick", "nick@bad.seeds").
pullreq("R3", "22", "closed_pr", prsts_closed, "done", "done@already.yo").
pullreq("R3", "33", "merged_pr", prsts_merged, "done", "done@already.yo").
pullreq("R6", "111", "blah", prsts_active, "nick", "nick@bad.seeds").
pullreq("R2", "1111", "blah", prsts_active, "not_nick", "not_nick@bad.seeds").
submodule("R1", project_primary, "master", "R2", "r2_master_head").
submodule("R1", project_primary, "master", "R3", "r3_master_head^3").
submodule("R1", project_primary, "master", "R4", "r4_master_head^1").
submodule("R1", project_primary, "feat1", "R2", "r2_master_head^1").
submodule("R1", project_primary, "feat1", "R3", "r3_master_head").
submodule("R1", project_primary, "feat1", "R4", "r4_feat1_head^2").
submodule("R1", "1", "blah", "R2", "r2_master_head^22").
submodule("R1", "1", "blah", "R3", "r3_master_head").
submodule("R1", "1", "blah", "R7", "r7_master_head^4").
varname("Project #1", "ghcver").
varname("Project #1", "c_compiler").
varvalue("Project #1", "ghcver", "ghc844").
varvalue("Project #1", "ghcver", "ghc865").
varvalue("Project #1", "ghcver", "ghc881").
varvalue("Project #1", "c_compiler", "gnucc").
varvalue("Project #1", "c_compiler", "clang").
'''.split('\n')))

# Note: the above does not contain branch("R2", "bugfix9").  This is
# because the optimization in InternalOps previously determined that
# bugfix9 was a pullreq on R2, so it suppressed the query.
