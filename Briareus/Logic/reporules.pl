/* Rules and reasoning -------------------------------------------------------------- */

%% Test if an argument is a Project Repo
is_project_repo(R) :- repo(R), project(R).

has_gitmodules(R, B) :-
    bagof(B, V^S^P^(is_project_repo(R), (branchreq(R,B); pullreq(R,_,B)), submodule(R, P, B, S, V)), BHG),
    \+ length(BHG, 0).

all_repos_no_subs(ALLR) :- findall(R, repo(R), ALLR).
all_repos(ALLR) :- findall(R, (repo(R) ; subrepo(R)), ALLR).
all_vars(ProjRepo, ALLV) :- findall(VN, varname(ProjRepo, VN), ALLV).

build_config(bldcfg(ProjRepo, BranchType, Branch, Strategy, BLDS, VARS)) :-
    is_project_repo(ProjRepo),
    branch_spec(BranchType, Branch, PR_ID),
    strategy(Strategy, ProjRepo, Branch),
    all_repos(RL),
    all_vars(ProjRepo, VL),
    varcombs(ProjRepo, VL, VARS),

    % If submodules and not master branch and the branch doesn't exist
    % in master, don't generate the configuration because the
    % submodules dictate all configurations so there's no way the
    % indicated branch can be referenced.
    (Branch == "master";
     BranchType == pullreq;
     Strategy == heads;
     Strategy == standard;
     (Branch \== "master",
      BranchType==regular,
      Strategy == submodules,
      all_repos_no_subs(TLR),
      branch_in_any(TLR, Branch)
     )),

    reporevs(RL, ProjRepo, BranchType, Branch, PR_ID, Strategy, BLDS)
.

branch_in_any([], _Branch) :- false.
branch_in_any([R|RL], Branch) :-
    branch(R, Branch) ; branch_in_any(RL, Branch).

varcombs(_, [], []).
varcombs(ProjRepo, [VN|VNS], [varvalue(ProjRepo,VN,VVS)|VNSVS]) :-
    varname(ProjRepo, VN),
    varvalue(ProjRepo, VN,VVS),
    varcombs(ProjRepo, VNS, VNSVS).

branch_type(pullreq, B) :- setof(X, R^I^pullreq(R, I, X), XS), member(B, XS).
branch_type(regular, B) :- branchreq(R, B), is_project_repo(R).

branch_spec(pullreq, B, I) :- setof((PI,PB), R^pullreq(R, PI, PB), XS), member((I,B), XS).
branch_spec(regular, B, project_primary) :- branchreq(R, B), is_project_repo(R).

useable_submodules(R, B) :- (branch(R, B), has_gitmodules(R, B));
                            (has_gitmodules(R, "master"), \+ branch(R, B)).  % KWQ: this doesn't work if reversed.

%% strategy(submodules, R, B) :- (branch_type(pullreq, B) ; branchreq(R, B)), useable_submodules(R, B).
%% strategy(heads,      R, B) :- (branch_type(pullreq, B) ; branchreq(R, B)), useable_submodules(R, B).
%% strategy(standard,   R, B) :- (branch_type(pullreq, B) ; branchreq(R, B)), \+ useable_submodules(R, B).
strategy(submodules, R, B) :- (branch_spec(pullreq, B, _I) ; branchreq(R, B)), useable_submodules(R, B).
strategy(heads,      R, B) :- (branch_spec(pullreq, B, _I) ; branchreq(R, B)), useable_submodules(R, B).
strategy(standard,   R, B) :- (branch_spec(pullreq, B, _I) ; branchreq(R, B)), \+ useable_submodules(R, B).


%% if pullreq changes submodules, don't have that data available
%% defaulting to master if unknown, but should default to origin of branch

reporevs([], _, _, _, _, _, []).
reporevs([R|Rs], ProjRepo, BranchType, Branch, PR_ID, Strategy, Result) :-
    (repo(R) ; subrepo(R)),
    reporevs(Rs, ProjRepo, BranchType, Branch, PR_ID, Strategy, RevSpecs),
    reporev(R, ProjRepo, BranchType, Branch, PR_ID, Strategy, RevSpec),
    build_revspecs(RevSpec, RevSpecs, Result),
    !  %% cut so that the reporevs below with build_revspecs(skip, ...) isn't used to skip this R.
.
%% If this is a subrepo that is not utilized on this ProjRepo branch, skip it
reporevs([R|Rs], ProjRepo, BranchType, Branch, PR_ID, Strategy, Result) :-
    subrepo(R),
    reporevs(Rs, ProjRepo, BranchType, Branch, PR_ID, Strategy, RevSpecs),
    build_revspecs(skip, RevSpecs, Result).


build_revspecs(RevSpec, RevSpecs, RevSpecs) :- RevSpec = skip.
build_revspecs(RevSpec, RevSpecs, [RevSpec|RevSpecs]) :- RevSpec \= skip.


%% branch_type = pullreq | regular
%% strategy = submodules | heads | standard

reporev(R, ProjRepo, pullreq, B, _PR_ID, submodules, RepoRev) :-
    submodule(ProjRepo, I, B, R, SubRev),
    pullreq(_, I, B),
    bldwith(RepoRev, R, SubRev, "project_primary", brr(09)).

reporev(R, ProjRepo, pullreq, B, _PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, I, B, R, _),
    pullreq(R, I, B),
    branch(R, B),
    bldwith(RepoRev, R, B, I, brr(08)).

reporev(R, ProjRepo, pullreq, B, _PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, I, B, R, _),
    pullreq(_, I, B),
    \+ branch(R, B),
    \+ pullreq(R, _I2, B),
    bldwith(RepoRev, R, "master", "project_primary", brr(07)).

reporev(R, ProjRepo, regular,  B, PR_ID, submodules, RepoRev) :-
    submodule(ProjRepo, project_primary, B, R, SubRev),
    bldwith(RepoRev, R, SubRev, PR_ID, brr(04)).

reporev(R, ProjRepo, _BType,  B, PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, project_primary, B, R, _),
    branch(R, B),
    bldwith(RepoRev, R, B, PR_ID, brr(05)).

reporev(R, ProjRepo, _BType,  B, PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, project_primary, B, R, _),
    \+ branch(R, B),
    branchreq(ProjRepo,B),
    bldwith(RepoRev, R, "master", PR_ID, brr(06)).

reporev(R, _ProjRepo, pullreq, B, PR_ID, _Strategy, RepoRev) :-
    repo(R),
    pullreq(R, I, B),
    ((B == "master", PR_ID == I); \+ B == "master"),
    bldwith(RepoRev, R, B, I, brr(03)).

reporev(R, ProjRepo, pullreq, B, _PR_ID, _Strategy,  RepoRev) :-
    submodule(ProjRepo, PI, "master", R, _),
    \+ pullreq(ProjRepo, PI, B),
    pullreq(R, I, B),
    bldwith(RepoRev, R, B, I, brr(10)).

reporev(R, ProjRepo, pullreq, B, PR_ID, submodules, RepoRev) :-
    submodule(ProjRepo, project_primary, "master", R, SubRev),
    \+ pullreq(ProjRepo, _, B),
    \+ pullreq(R, PR_ID, B),
    bldwith(RepoRev, R, SubRev, "project_primary", brr(11)).

reporev(R, ProjRepo, pullreq, B, PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, project_primary, "master", R, _),
    \+ pullreq(ProjRepo, _, B),
    \+ pullreq(R, PR_ID, B),
    bldwith(RepoRev, R, "master", "project_primary", brr(12)).

reporev(R, ProjRepo, regular, B, PR_ID, submodules, RepoRev) :-
    submodule(ProjRepo, I, "master", R, SubRev),
    \+ submodule(ProjRepo, I, B, R, _),
    \+ branch(R, B),
    bldwith(RepoRev, R, SubRev, PR_ID, brr(13)),
    !.

reporev(R, ProjRepo, regular, B, PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, I, "master", R, _),
    \+ submodule(ProjRepo, I, B, R, _),
    branch(R, B),
    bldwith(RepoRev, R, B, PR_ID, brr(15)),
    !.

reporev(R, ProjRepo, regular, B, PR_ID, heads, RepoRev) :-
    submodule(ProjRepo, I, "master", R, _),
    \+ submodule(ProjRepo, I, B, R, _),
    \+ branch(R, B),
    bldwith(RepoRev, R, "master", PR_ID, brr(14)),
    !.

reporev(R, ProjRepo, _BType,  B, _PR_ID, _Strategy,  RepoRev) :-
    repo(R),
    \+ submodule(ProjRepo, _I, B, R, _),
    branch(R, B),  % KWQ swap with above
    bldwith(RepoRev, R, B, "project_primary", brr(01)).

reporev(R, ProjRepo, _BType,  B, _PR_ID, _Strategy,  RepoRev) :-
    repo(R),
    \+ submodule(ProjRepo, _I, B, R, _),
    \+ branch(R, B),
    bldwith(RepoRev, R, "master", "project_primary", brr(02)).

bldwith(bld(R, B, I, T), R, B, I, T).
