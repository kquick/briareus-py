%% This provides a library of various logic predicates that can be
%% used in the generation and analysis of build configurations.

%% Test if an argument is a Project Repo
is_project_repo(R) :- repo(R, R), project(R).

is_main_branch(Repo, Branch) :-
    branch(Repo, Branch),
    (main_branch(Repo, Branch) ; (default_main_branch(Branch), \+ main_branch(Repo, _))).

all_repos_no_subs(ProjRepo, ALLR) :- findall(R, (repo(ProjRepo, R), \+ subrepo(ProjRepo, R)), ALLR).
all_repos(ProjRepo, ALLR) :- findall(R, (repo(ProjRepo, R) ; subrepo(ProjRepo, R)), ALLR).
all_vars(ProjRepo, ALLV) :- findall(VN, varname(ProjRepo, VN), ALLV).

% proj_repo_branch: does the branch exist for the specified project?
proj_repo_branch(R, B) :- is_project_repo(R), branchreq(R, B).
proj_repo_branch(R, B) :- is_project_repo(R), is_main_branch(R, B), \+ branchreq(R, B).

% ----------------------------------------------------------------------
% Branch Type

branch_type(pullreq, B, PR_ID) :-
    setof((PI,PB), R^pullreq(R, PI, PB), XS)
    , member((PR_ID,B), XS)
.
branch_type(regular, B, project_primary) :-
    setof(BR, R^proj_repo_branch(R,BR), BRS)
    , member(B, BRS)
.


% ----------------------------------------------------------------------
% Build Strategies

strategy_plan(submodules, R, B) :-
    (branch_type(pullreq, B, _I) ; branchreq(R, B); is_main_branch(R, B))
    , useable_submodules(R, B)
.
strategy_plan(heads,      R, B) :-
    (branchreq(R, B); is_main_branch(R, B))
    , useable_submodules(R, B)
.
strategy_plan(heads,      R, B) :-
    branch_type(pullreq, B, _I)
    , submodule(R, _I2, _B, _SR, _SRRef)
.
strategy_plan(standard,   R, B) :-
    (branch_type(pullreq, B, _I)
    ; branchreq(R, B)
    ; is_main_branch(R, B)
    )
    , \+ strategy_plan(heads, R, B)
.

useable_submodules(R, B) :-
    (branch(R, B), has_gitmodules(R, B));
    (is_main_branch(R, MB), has_gitmodules(R, MB), \+ branch(R, B)).

has_gitmodules(R, B) :-
    bagof(B, V^S^P^(is_project_repo(R), (proj_repo_branch(R, B); pullreq(R,_,B)), submodule(R, P, B, S, V)), BHG),
    \+ length(BHG, 0).

strategy(S, R, B) :- setof(ST, strategy_plan(ST,R,B), SS), member(S, SS).

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
varcombs(ProjRepo, [VN|VNS], [varvalue(ProjRepo,VN,VVS)|VNSVS]) :-
    varname(ProjRepo, VN),
    varvalue(ProjRepo, VN,VVS),
    varcombs(ProjRepo, VNS, VNSVS).

