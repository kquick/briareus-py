:- consult(buildlib).

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
    pullreq(Repo, PRNum, Branch),
    is_main_branch(Repo, Branch),
    findall(R, (pullreq(R, PRNum, Branch), is_main_branch(R, Branch)), RS),
    length(RS, RLen),
    RLen < 2.

pr_type(pr_repogroup, PRNum, RepoList) :-
    % pullreq(_R, PRNum, Branch),
    % is_main_branch(Repo, Branch),
    setof(R, (pullreq(R, PRNum, Branch), is_main_branch(R, Branch)), RepoList),
    length(RepoList, RLen),
    RLen > 1.

pr_type(pr_grouped, BranchName) :-
    setof(B, R^I^pullreq(R, I, B), BS),
    member(BranchName, BS),
    findall((R,I), (pullreq(R,I,BranchName), \+ is_main_branch(R,BranchName)), PRList),
    length(PRList, NumPRs),
    NumPRs > 0.

% A pr_config describes a configuration for a pull request; this is a
% subset of a build configuration that primarily describes the
% PR-related information.  The PRCfg output is an array of
%     prcfg(Repo, PRNum, Branch)
%     branchcfg(Repo, BranchName)
% items.
pr_config(pr_type(pr_solo, Repo, PRNum), PRCfg) :-
    pr_type(pr_solo, Repo, PRNum),
    pullreq(Repo, PRNum, Branch),
    PRCfg = [ prcfg(Repo, PRNum, Branch) ].

pr_config(pr_type(pr_repogroup, PRNum, RepoList), PRCfg) :-
    pr_type(pr_repogroup, PRNum, RepoList),
    % the Branch should be the same main_branch for all prcfgs since
    % they refer to the same actual repo
    setof(R, Repo^(is_main_branch(Repo, Branch),
                   pullreq(Repo, PRNum, Branch),
                   R = prcfg(Repo, PRNum, Branch)),
          PRCfg).

pr_config(pr_type(pr_grouped, BranchName), PRCfg) :-
    pr_type(pr_grouped, BranchName),
    setof(R, Repo^I^( (pullreq(Repo, I, BranchName), R = prcfg(Repo, I, BranchName))
                    ; (branch(Repo, BranchName),
                       \+pullreq(Repo, I, BranchName), R = branchcfg(Repo, BranchName))),
          PRCfg).
