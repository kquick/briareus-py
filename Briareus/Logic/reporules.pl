/* Rules and reasoning -------------------------------------------------------------- */

%% Test if an argument is a Project Repo
is_project_repo(R) :- repo(R), project(R).

has_gitmodules(R, B) :-
    bagof(B, V^S^(is_project_repo(R), (branchreq(R,B); pullreq(R,_,B)), submodule(R, B, S, V)), BHG),
    \+ length(BHG, 0).

all_repos_no_subs(ALLR) :- findall(R, repo(R), ALLR).
all_repos(ALLR) :- findall(R, (repo(R) ; subrepo(R)), ALLR).
all_vars(ALLV) :- findall(VN, varname(VN), ALLV).

build_config(bldcfg(BranchType, Branch, Strategy, BLDS, VARS)) :-
    branch_type(BranchType, Branch),
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    all_repos(RL),
    all_vars(VL),
    varcombs(VL, VARS),
    reporevs(RL, ProjRepo, BranchType, Branch, Strategy, BLDS)
.

varcombs([], []).
varcombs([VN|VNS], [var(VN,VVS)|VNSVS]) :-
    varname(VN),
    var(VN,VVS),
    varcombs(VNS, VNSVS).

branch_type(pullreq, B) :- pullreq(_, _, B).
branch_type(regular,  B) :- branchreq(R, B), is_project_repo(R).

strategy(submodules, R, B) :-    branch(R, B), has_gitmodules(R, B).
strategy(heads,      R, B) :-    branch(R, B), has_gitmodules(R, B).
strategy(submodules, R, B) :- \+ branch(R, B), has_gitmodules(R, "master").
strategy(heads,      R, B) :- \+ branch(R, B), has_gitmodules(R, "master").
strategy(submodules, R, B) :-    pullreq(_, _, B), has_gitmodules(R, "master").
strategy(heads,      R, B) :-    pullreq(_, _, B), has_gitmodules(R, "master").
strategy(main,       R, B) :- \+ strategy(heads, R, B).

%% if pullreq changes submodules, don't have that data available
%% defaulting to master if unknown, but should default to origin of branch

reporevs([], _, _, _, _, []).
reporevs([R|Rs], ProjRepo, BranchType, Branch, Strategy, Result) :-
    (repo(R) ; subrepo(R)),
    reporevs(Rs, ProjRepo, BranchType, Branch, Strategy, RevSpecs),
    reporev(R, ProjRepo, BranchType, Branch, Strategy, RevSpec),
    build_revspecs(RevSpec, RevSpecs, Result),
    !
.
%% If this is a subrepo that is not utilized on this ProjRepo branch, skip it
reporevs([R|Rs], ProjRepo, BranchType, Branch, Strategy, Result) :-
    subrepo(R),
    reporevs(Rs, ProjRepo, BranchType, Branch, Strategy, RevSpecs),
    build_revspecs(skip, RevSpecs, Result).


build_revspecs(RevSpec, RevSpecs, RevSpecs) :- RevSpec = skip.
build_revspecs(RevSpec, RevSpecs, [RevSpec|RevSpecs]) :- RevSpec \= skip.


%% branch_type = pullreq | regular
%% strategy = submodules | heads | main

reporev(R, ProjRepo, pullreq, B, submodules, RepoRev) :- submodule(ProjRepo, B, R, SubRev), pullreq(_, _I, B),                   bldwith(RepoRev, R, SubRev, brr(09)).
reporev(R, ProjRepo, pullreq, B, heads,      RepoRev) :- submodule(ProjRepo, B, R, _),      pullreq(_, _I, B),    branch(R, B),  bldwith(RepoRev, R, B, brr(08)).
reporev(R, ProjRepo, pullreq, B, heads,      RepoRev) :- submodule(ProjRepo, B, R, _),      pullreq(_, _I, B), \+ branch(R, B),  bldwith(RepoRev, R, "master", brr(07)).
reporev(R, ProjRepo, _BType,  B, submodules, RepoRev) :- submodule(ProjRepo, B, R, SubRev),                            bldwith(RepoRev, R, SubRev, brr(04)).
reporev(R, ProjRepo, _BType,  B, heads,      RepoRev) :- submodule(ProjRepo, B, R, _),    branch(R, B),                bldwith(RepoRev, R, B, brr(05)).
reporev(R, ProjRepo, _BType,  B, heads,      RepoRev) :- submodule(ProjRepo, B, R, _), \+ branch(R, B), branchreq(ProjRepo,B),     bldwith(RepoRev, R, "master", brr(06)).
reporev(R, _ProjRepo, pullreq, B, _Strategy, RepoRev) :- repo(R), pullreq(R, _I, B),                                   bldwith(RepoRev, R, B, brr(03)).
reporev(R, ProjRepo, pullreq, B, _Strategy,  RepoRev) :- submodule(ProjRepo, "master", R, _),      \+ pullreq(ProjRepo, _, B),    pullreq(R, _, B), bldwith(RepoRev, R, B, brr(10)).
reporev(R, ProjRepo, pullreq, B, submodules, RepoRev) :- submodule(ProjRepo, "master", R, SubRev), \+ pullreq(ProjRepo, _, B), \+ pullreq(R, _, B), bldwith(RepoRev, R, SubRev, brr(11)).
reporev(R, ProjRepo, pullreq, B, heads,      RepoRev) :- submodule(ProjRepo, "master", R, _),      \+ pullreq(ProjRepo, _, B), \+ pullreq(R, _, B), bldwith(RepoRev, R, "master", brr(12)).
reporev(R, ProjRepo, regular, B, submodules, RepoRev) :- \+ submodule(ProjRepo, B, R, _), submodule(ProjRepo, "master", R, SubRev),  \+ branch(R, B), bldwith(RepoRev, R, SubRev, brr(13)), !.
reporev(R, ProjRepo, regular, B, heads,      RepoRev) :- \+ submodule(ProjRepo, B, R, _), submodule(ProjRepo, "master", R, _),  \+ branch(R, B), bldwith(RepoRev, R, "master", brr(14)), !.
reporev(R, ProjRepo, _BType,  B, _Strategy,  RepoRev) :- repo(R), \+ submodule(ProjRepo, B, R, _),    branch(R, B),    bldwith(RepoRev, R, B, brr(01)).
reporev(R, ProjRepo, _BType,  B, _Strategy,  RepoRev) :- repo(R), \+ submodule(ProjRepo, B, R, _), \+ branch(R, B),    bldwith(RepoRev, R, "master", brr(02)).


bldwith(bld(R, B, I), R, B, I).
