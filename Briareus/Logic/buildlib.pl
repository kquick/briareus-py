%% This provides a library of various logic predicates that can be
%% used in the generation and analysis of build configurations.

listcmp(AS, BS) :- length(AS, L), length(BS, L), listcmp_(AS, BS).
listcmp_([A|AS], BS) :- member(A, BS), listcmp_(AS, BS).
listcmp_([], _).

unzip([], [], []).
unzip([(A,B)|ABS], [A|AS], [B|BS]) :- unzip(ABS, AS, BS).


%% Test if an argument is a Project Repo.  A single match is sufficient.
is_project_repo(R) :- project(_, R), !.

is_main_branch(Repo, Branch) :-
    branch(Repo, Branch),
    (main_branch(Repo, Branch) ; (default_main_branch(Branch), \+ main_branch(Repo, _))).

all_repos_no_subs(PName, ALLR) :-
    project(PName, ProjRepo),
    findall(R, (repo(PName, R), \+ subrepo(ProjRepo, R)), ALLR).
% all_repos(ProjRepo, ALLR) :- findall(R, (repo(ProjRepo, R) ; subrepo(ProjRepo, R)), ALLR).
all_vars(PName, ALLV) :- findall(VN, varname(PName, VN), ALLV).

repo_in_project(PName, Repo) :-
    project(PName, ProjRepo)
    , setof(R, (repo(PName, R) ; subrepo(ProjRepo, R)), RS)
    , member(Repo, RS)
.

% proj_repo_branch: does the branch exist for the specified project?
proj_repo_branch(PName, B) :- branchreq(PName, B).
proj_repo_branch(PName, B) :- project(PName, R), is_main_branch(R, B), \+ branchreq(PName, B).

% ----------------------------------------------------------------------
% Branch Type

:- table branch_type/3.

branch_type(pullreq, B, PR_ID) :-
    setof((PI,PB), R^pullreq(R, PI, PB, _, _, _, _), XS)
    , member((PR_ID,B), XS)
.
branch_type(regular, B, project_primary) :-
    setof(BR, N^proj_repo_branch(N,BR), BRS)
    , member(B, BRS)
.


% ----------------------------------------------------------------------
% Build Strategies

:- table strategy_plan/3.

strategy_plan(submodules, PName, B) :-
    project(PName, R)
    , (branch_type(pullreq, B, _I) ; branchreq(PName, B); is_main_branch(R, B))
    , useable_submodules(PName, R, B)
.
strategy_plan(heads, PName, B) :-
    project(PName, R)
    , (branchreq(PName, B); is_main_branch(R, B))
    , useable_submodules(PName, R, B)
.
strategy_plan(heads, PName, B) :-
    project(PName, R)
    , branch_type(pullreq, B, _I)
    , submodule(R, _I2, _B, _SR, _SRRef)
.
strategy_plan(standard, PName, B) :-
    project(PName, R)
    , (branch_type(pullreq, B, _I)
      ; branchreq(PName, B)
      ; is_main_branch(R, B)
    )
    , \+ strategy_plan(heads, PName, B)
.

:- table useable_submodules/3.

useable_submodules(PName, R, B) :-
    (branch(R, B), has_gitmodules(PName, R, B));
    (is_main_branch(R, MB), has_gitmodules(PName, R, MB), \+ branch(R, B)).

:- table has_gitmodules/3.

has_gitmodules(PName, R, B) :-
    project(PName, R)
    , is_project_repo(R)
    , bagof(S, V^P^((proj_repo_branch(PName, B) ; pullreq(R,_,B,_,_,_,_))
                    , submodule(R, P, B, S, V))
            , SBG)
    , \+ length(SBG, 0)
.

:- table strategy/3.

strategy(S, PName, B) :- setof(ST, strategy_plan(ST,PName,B), SS), member(S, SS).

% ----------------------------------------------------------------------
% Variable Value combinations

% Given a Project and a list of variable names, return each array of
% variable values for each of the variable names.  If there are two
% variables, and each can have two values, then varcombs succeeds four
% times, with:
%
%     [varvalue(P,V1,V1Val1), varvalue(P,V2,V2Val1)]
%
%     [varvalue(P,V1,V1Val1), varvalue(P,V2,V2Val2)]
%
%     [varvalue(P,V1,V1Val2), varvalue(P,V2,V2Val1)]
%
%     [varvalue(P,V1,V1Val2), varvalue(P,V2,V2Val2)]
%
varcombs(_, [], []).
varcombs(PName, [VN|VNS], [varvalue(PName,VN,VVS)|VNSVS]) :-
    varname(PName, VN),
    varvalue(PName, VN,VVS),
    varcombs(PName, VNS, VNSVS).
