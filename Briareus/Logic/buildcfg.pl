:- consult(buildlib).
:- consult(pullreqinfo).

build_config2(bldcfg(ProjRepo, pullreq, Branch, Strategy, Cfg, BLDS, VARS)) :-
    is_project_repo(ProjRepo)
    , pr_config(Cfg, CFG)
    , branch_for_prtype(ProjRepo, Cfg, Branch)
    , finish_config(ProjRepo, CFG, Branch, Strategy, BLDS, VARS)
    .

build_config2(bldcfg(ProjRepo, BranchType, Branch, Strategy, Cfg, BLDS, VARS)) :-
    is_project_repo(ProjRepo)
    % Note that there might also be a pullreq for this branch, but the
    % pullreq is typically from a forked repo, and this branch was
    % specifically requested, so make the branchreq build distinct
    % from the pullreq build.
    , branchreq(ProjRepo, Branch)
    % , \+ branch_covered_by_pullreq(Branch)
    , Cfg = branchreq(ProjRepo, Branch)
    , BranchType = regular
    , CFG = []
    , finish_config(ProjRepo, CFG, Branch, Strategy, BLDS, VARS)
    , is_branch_reachable(ProjRepo, Strategy, Branch)
    .

build_config2(bldcfg(ProjRepo, BranchType, Branch, Strategy, Cfg, BLDS, VARS)) :-
    is_project_repo(ProjRepo)
    , is_main_branch(ProjRepo, Branch)
    , \+ branchreq(ProjRepo, Branch)
    , Cfg = is_main_branch(ProjRepo, Branch)
    , BranchType = regular
    , CFG = []
    , finish_config(ProjRepo, CFG, Branch, Strategy, BLDS, VARS)
    , is_branch_reachable(ProjRepo, Strategy, Branch)
    .

finish_config(ProjRepo, CFG, Branch, Strategy, BLDS, VARS) :-
    strategy(Strategy, ProjRepo, Branch)
    , pr_builds(CFG, PRBLDS)
    , pr_builds_proj(CFG, ProjRepo, Branch, Proj_PR_ID, ProjBranch)
    , rem_builds(ProjRepo, Strategy, Branch, Proj_PR_ID, ProjBranch, PRBLDS, REMBLDS)
    , join_prblds_remblds(PRBLDS, REMBLDS, BLDS)
    , all_vars(ProjRepo, VL)
    , varcombs(ProjRepo, VL, VARS)
    .

join_prblds_remblds(PRBLDS, REMBLDS, BLDS) :-
    % In general, these are two distinct sets: the PRBLDS are
    % determined by any PR associations, and then the REMBLDS are the
    % configurations for the remaining repos not covered by the PR
    % associations.  However, in certain cases
    % (e.g. submodules-respecting builds), the REMBLDS may have
    % determined an override for a member of PRBLDS, so the join of
    % the two should prefer REMBLDS entries over PRBLDS entries when
    % there is a repo overlap.
    builds_repos(REMBLDS, REMREPOS)
    , exclude(bld_repo_in(REMREPOS), PRBLDS, PRBLDS_USE)
    , append(PRBLDS_USE, REMBLDS, BLDS)
    .

bld_repo_in(RLIST, BLD) :- build_repo(BLD, R), member(R, RLIST).

branch_for_prtype(ProjRepo, pr_type(pr_solo, Repo, _), Branch) :-
    repo_in_project(ProjRepo, Repo)
    , is_main_branch(Repo, Branch)
.
branch_for_prtype(ProjRepo, pr_type(pr_repogroup, _, RepoList), Branch) :-
    maplist(repo_in_project(ProjRepo), RepoList)
    , nth0(0, RepoList, R0)
    , is_main_branch(R0, Branch)
.
branch_for_prtype(_ProjRepo, pr_type(pr_grouped, Branch), Branch).

is_branch_reachable(ProjRepo, standard,   Branch) :- repo(ProjRepo, R), branch(R, Branch), !.
is_branch_reachable(_PrjRepo, heads,      _Branch). % :- repo(ProjRepo, R), branch(R, Branch), !.  % KWQ: increases failures
is_branch_reachable(ProjRepo, submodules, Branch) :-
    branch(R, Branch)
    , all_repos_no_subs(ProjRepo, TLR)
    , member(R, TLR)
    , !  % only necessary to find one case.
.


repo_in_project(ProjRepo, Repo) :-
    setof(R, (repo(ProjRepo, R) ; subrepo(ProjRepo, R)), RS),
    member(Repo, RS).

pr_builds([], []).
pr_builds([prcfg(R,I,B)|PRCFGS], [bld(R,B,I,brr(31))|BLDS]) :-
    pr_builds(PRCFGS, BLDS).
pr_builds([branchcfg(R,B)|PRCFGS], [bld(R,B,project_primary,brr(30))|BLDS]) :-
    pr_builds(PRCFGS, BLDS).

pr_builds_proj([], ProjRepo, Branch, project_primary, Branch) :- branch(ProjRepo, Branch), !.
pr_builds_proj([], ProjRepo, _Branch, project_primary, ProjBranch) :- is_main_branch(ProjRepo, ProjBranch).
pr_builds_proj([prcfg(ProjRepo,I,ProjBranch)|_PRCFGS], ProjRepo, _Branch, I, ProjBranch) :- !.
pr_builds_proj([branchcfg(ProjRepo,ProjBranch)|_PRCFGS], ProjRepo, _Branch, project_primary, ProjBranch) :- !.
pr_builds_proj([_|PRCFGS], ProjRepo, Branch, Proj_PR_ID, ProjBranch) :-
    pr_builds_proj(PRCFGS, ProjRepo, Branch, Proj_PR_ID, ProjBranch).


builds_repos(PRBLDS, REPOS) :- maplist(build_repo, PRBLDS, REPOS).
build_repo(bld(Repo, _, _, _), Repo).

rem_builds(ProjRepo, Strategy, Branch, Proj_PR_ID, ProjBranch, PRBLDS, REMBLDS) :-
    builds_repos(PRBLDS, PRREPOS),
    findall(BLD, rem_build(ProjRepo, Strategy, Branch, Proj_PR_ID, ProjBranch, PRREPOS, BLD), REMBLDS).
%% KWQ: setof the above for uniqueness, but needed findall because there may not be any repos not already in PRBLDS

rem_build(ProjRepo, standard, Branch, _, _, PRBLD_REPOS, bld(R, B, project_primary, brr(33))) :-
    repo_in_project(ProjRepo, R)  % projbranch is a pr, no submodules, so other repos should fallback to main
    , \+ member(R, PRBLD_REPOS)
    , rem_build_branch(Branch, R, B).

rem_build(ProjRepo, heads, Branch, Proj_PR_ID, ProjBranch, PRBLD_REPOS, bld(R, B, project_primary, brr(32))) :-
    repo_in_project(ProjRepo, R)
    , repo_useable(ProjRepo, Proj_PR_ID, ProjBranch, R)
    , \+ member(R, PRBLD_REPOS)
    , rem_build_branch(Branch, R, B)
.

rem_build(ProjRepo, submodules, Branch, Proj_PR_ID, ProjBranch, PRBLD_REPOS, bld(R, B, project_primary, brr(34))) :-
    repo_in_project(ProjRepo, R)
    , repo_useable(ProjRepo, Proj_PR_ID, ProjBranch, R)
    % In general, PRBLD_REPOS has been identified as all the repos
    % where the PR exists, and rem_build is only collecting repos that
    % don't exist in the associated PR's (PRBLD_REPOS).
    %
    % Normally, the existence of a PR overrides a submodule lock,
    % because the intent of the pullreq build is to find out of the
    % pullreq can be merged (the assumption is that the primary repo's
    % submodules will be updated at the same time that the PR is
    % merged... the analysis/notifications portion is expected to
    % handle this).
    %
    % However, if there is a corresponding PR (i.e. one with this same
    % PR branch) on the ProjRepo, then the submodule specifications
    % are strictly adhered to.  The understanding is that by opening
    % the PR with this branchname on the ProjRepo that changes are
    % being made for this PR set, including updating the submodules.
    , submod_spec_or_branch(ProjRepo, Proj_PR_ID, ProjBranch, PRBLD_REPOS, Branch, R, B)
.

submod_spec_or_branch(ProjRepo, Proj_PR_ID, ProjBranch, _, ProjBranch, Repo, B) :-
    submodule(ProjRepo, Proj_PR_ID, ProjBranch, Repo, B)
    , Proj_PR_ID \== project_primary
    , !
.
submod_spec_or_branch(ProjRepo, Proj_PR_ID, ProjBranch, PRBLD_REPOS, _BldBranch, Repo, B) :-
    submodule(ProjRepo, Proj_PR_ID, ProjBranch, Repo, B)
    % , BldBranch \== ProjBranch
    , \+ member(Repo, PRBLD_REPOS)
    , !
.
submod_spec_or_branch(_, _, _, PRBLD_REPOS, BldBranch, Repo, B) :-
    rem_build_branch(BldBranch, Repo, B)
    , \+ member(Repo, PRBLD_REPOS)
.


rem_build_branch(BldBranch, Repo, BldBranch) :-
    branch(Repo, BldBranch), !.
rem_build_branch(_BldBranch, Repo, Branch) :-
    is_main_branch(Repo, Branch).

repo_useable(ProjRepo, _Proj_PR_ID, _ProjBranch, Repo) :-
    repo(ProjRepo, Repo)   % user specified, so always visible
    , !
.
repo_useable(ProjRepo, Proj_PR_ID, ProjBranch, Repo) :-
    subrepo(ProjRepo, Repo)
    , submodule(ProjRepo, Proj_PR_ID, ProjBranch, Repo, _)
.
