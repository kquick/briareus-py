:- consult(buildlib).

% ----------------------------------------------------------------------
% PR Configuration Types

% A pr_solo is a PR that should not be built with any other PRs,
% because the PR is against the main branch (probably the repo was
% forked and the PR generated from the fork without creating a branch
% first).  It doesn't make sense to combine these PR's because they
% are probably unrelated, so build each one separately.
%
% Conversely, if the PR is not on the main branch, and that same
% branch name exists in other repos, then this probably indicates that
% the branches should be built together because they represent
% coordinated changes across multiple repos.  This is represented by
% the pr_grouped type.
%
% An additional consideration is that multiple "Repos" may actually
% reference the same top-level repository: one which contains multiple
% build modules in different sub-directories.  For a repo_group, the
% pr_grouped does not change (all the repos in the group will share
% the same PR + branch and will be built together), but for a pr_solo
% it means that all the repos in a repo_group should be built
% together, although independently from other repos.  This is
% identified by the identical PRNum and Branch (which is therefore
% assumed to be a repo group) and represented by the pr_repogroup type
% instead of the pr_solo type.
%
% Special cases:
%
%  1 - same repo, multiple PRs, same branch name
%
%  2 - one Repo uses non-standard main_branch that matches the branch
%  name of a PR in a different Repo.
%
% Case 1: not handled; will likely be unstable and alternate between PRs.
%
% Case 2: pr_grouped for all repos except non-standard main branch.
%         If any PR's exist on that repo on the main branch, they are
%         pr_solo.
pr_type(pr_solo, Repo, PRNum) :-
    pullreq(Repo, PRNum, Branch, _, _),
    is_main_branch(Repo, Branch),
    findall(R, (pullreq(R, PRNum, Branch, _U, _E), is_main_branch(R, Branch)), RS),
    length(RS, RLen),
    RLen < 2.

pr_type(pr_repogroup, PRNum, RepoList) :-
    setof(R, (pullreq(R, PRNum, Branch, _, _), is_main_branch(R, Branch)), RepoList),
    length(RepoList, RLen),
    RLen > 1.

pr_type(pr_grouped, BranchName) :-
    setof(B, R^I^U^E^pullreq(R, I, B, U, E), BS),
    member(BranchName, BS),
    findall((R,I), (pullreq(R,I,BranchName,_Uu,_Ee), \+ is_main_branch(R,BranchName)), PRList),
    length(PRList, NumPRs),
    NumPRs > 0.

% ----------------------------------------------------------------------
% PR Configurations

% A pr_config describes a configuration for a pull request; this is a
% subset of a build configuration that primarily describes the
% PR-related information.  The PRCfg output is an array of
%     prcfg(Repo, PRNum, Branch, User, Email)
%     branchcfg(Repo, BranchName)
% items.
pr_config(pr_type(pr_solo, Repo, PRNum), ProjName, PRCfg) :-
    pr_type(pr_solo, Repo, PRNum)
    , pullreq(Repo, PRNum, Branch, User, Email)
    , repo_in_project(ProjName, Repo)
    , PRCfg = [ prcfg(Repo, PRNum, Branch, User, Email) ]
.

pr_config(pr_type(pr_repogroup, PRNum, RepoList), ProjName, PRCfg) :-
    pr_type(pr_repogroup, PRNum, RepoList),
    % the Branch should be the same main_branch for all prcfgs since
    % they refer to the same actual repo
    setof(R, Repo^(is_main_branch(Repo, Branch)
                  , pullreq(Repo, PRNum, Branch, User, Email)
                  , repo_in_project(ProjName, Repo)
                  , R = prcfg(Repo, PRNum, Branch, User, Email))
          , PRCfg).

pr_config(pr_type(pr_grouped, BranchName), ProjName, PRCfg) :-
    pr_type(pr_grouped, BranchName)
    , findall(R, (pullreq(Repo, I, BranchName, User, Email)
                  , repo_in_project(ProjName, Repo)
                  , R = prcfg(Repo, I, BranchName, User, Email))
              , PRCfg_PR)
    , findall(R, (branch(Repo, BranchName)
                  , repo_in_project(ProjName, Repo)
                  , \+pullreq(Repo, I, BranchName, _U, _E)
                  , R = branchcfg(Repo, BranchName))
              , PRCfg_BR)
    , append(PRCfg_PR, PRCfg_BR, PRCfg_All)
    , list_to_set(PRCfg_All, PRCfg)
.

% ----------------------------------------------------------------------
% Misc support

branch_for_prtype(pr_type(pr_solo, Repo, _), Branch) :-
    is_main_branch(Repo, Branch)
.
branch_for_prtype(pr_type(pr_repogroup, _, RepoList), Branch) :-
    nth0(0, RepoList, R0)
    , is_main_branch(R0, Branch)
.
branch_for_prtype(pr_type(pr_grouped, Branch), Branch).
