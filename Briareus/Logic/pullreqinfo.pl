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

:- table pr_type/4.
:- table pr_type/2.

pr_type(pr_solo, Project, Repo, PRNum) :-
    repo_in_project(Project, Repo)
    , pullreq(Repo, PRNum, Branch, _, Sts, _, _)
    , active_prsts(Sts)
    , is_main_branch(Repo, Branch)
    % If this should be a repogroup PR instead, there will be multiple
    % pullreq facts with the same PRNum.
    , findall(R, (pullreq(R, PRNum, Branch, _, S, _U, _E)
                  , active_prsts(S)
                  , repo_in_project(Project, R)
                  , is_main_branch(R, Branch)
                 )
              , RS)
    , length(RS, 1)
.

pr_type(pr_repogroup, Project, PRNum, RepoList) :-
    % The same PRNum binds the pullreqs to the same repo
    setof(R, Ref^User^Email^(pullreq(R, PRNum, Branch, Ref, Sts, User, Email)
                             , active_prsts(Sts)
                             , repo_in_project(Project, R)
                             , is_main_branch(R, Branch)
                            )
          , RepoList)
    , length(RepoList, RLen)
    , RLen > 1
.

pr_type(pr_grouped, BranchName) :-
    setof(B, R^I^U^E^F^S^(pullreq(R, I, B, F, S, U, E), active_prsts(S)), BS)
    , member(BranchName, BS)
    , findall((R,I), (pullreq(R,I,BranchName,_Ref,Sts,_Uu,_Ee)
                      , active_prsts(Sts)
                      , \+ is_main_branch(R,BranchName)
                     )
              , PRList)
    , length(PRList, NumPRs)
    , NumPRs > 0.

% ----------------------------------------------------------------------
% PR Configurations

% A pr_config describes a configuration for a pull request; this is a
% subset of a build configuration that primarily describes the
% PR-related information.  The PRCfg output is an array of
%     prcfg(Repo, PRNum, Branch, BranchRef, User, Email)
%     branchcfg(Repo, BranchName)
% items.

:- table pr_config/3.

pr_config(pr_type(pr_solo, ProjName, Repo, PRNum), ProjName, PRCfg) :-
    active_prsts(Sts)
    , pullreq(Repo, PRNum, Branch, BranchRef, Sts, User, Email)
    , repo_in_project(ProjName, Repo)
    , PRType = pr_type(pr_solo, ProjName, Repo, PRNum)
    , PRType
    , PRCfg = [ prcfg(Repo, PRNum, Branch, BranchRef, User, Email) ]
.

pr_config(PRType, ProjName, PRCfg) :-
    PRType = pr_type(pr_repogroup, ProjName, PRNum, _RepoList)
    , PRType
    % ensure at least one cfg for this pr, as well as establishing the
    % ProjName output
    , pullreq_active_in_project(PRType, ProjName)
    % the Branch should be the same main_branch for all prcfgs since
    % they refer to the same actual repo
    , setof(Res, R^(is_main_branch(R, Br)
                    , pullreq(R, PRNum, Br, BRef, Sts, Usr, EmailAddr)
                    , active_prsts(Sts)
                    , repo_in_project(ProjName, R)
                    , Res = prcfg(R, PRNum, Br, BRef, Usr, EmailAddr))
            , PRCfg)
.

pr_config(PRType, ProjName, PRCfg) :-
    PRType = pr_type(pr_grouped, BranchName)
    , PRType
    % establish ProjName output and ensure at least one PRCfg
    , project(ProjName)
    , pullreq_active_in_project(PRType, ProjName)
    % Collect all PRCfg
    , findall(R, (pullreq(Repo, I, BranchName, BranchRef, S, User, Email)
                  , active_prsts(S)
                  , repo_in_project(ProjName, Repo)
                  , R = prcfg(Repo, I, BranchName, BranchRef, User, Email))
              , PRCfg_PR)
    , findall(R, (branch(Repo, BranchName)
                  , repo_in_project(ProjName, Repo)
                  , \+pullreq(Repo, I, BranchName, _, Sts, _U, _E)
                  , active_prsts(Sts)
                  , R = branchcfg(Repo, BranchName))
              , PRCfg_BR)
    , append(PRCfg_PR, PRCfg_BR, PRCfg_All)
    , list_to_set(PRCfg_All, PRCfg)
.


% ----------------------------------------------------------------------
% Misc support

active_prsts(prsts_new).
active_prsts(prsts_active).


% pullreq_active_in_project is true if the PRType is valid and in
% active status for the specified Project.
pullreq_active_in_project(PRType, Project) :-
    PRType = pr_type(pr_grouped, Branch)
    , PRType
    , project(Project)
    , repo_in_project(Project, ProjRepo)
    , active_prsts(Status)
    , pullreq(ProjRepo, _, Branch, _, Status, _, _)
    , !  % just one is necessary
.
pullreq_active_in_project(PRType, Project) :-
    PRType = pr_type(pr_repogroup, Project, PRNum, _)
    , PRType
    , project(Project)
    , repo_in_project(Project, Repo)
    , is_main_branch(Repo, Branch)
    , active_prsts(Status)
    , pullreq(Repo, PRNum, Branch, _, Status, _, _)
    , !  % just one is necessary
.
pullreq_active_in_project(PRType, Project) :-
    PRType = pr_type(pr_solo, Project, Repo, PRNum)
    , PRType
    , project(Project)
    , repo_in_project(Project, Repo)
    , active_prsts(Status)
    , pullreq(Repo, PRNum, _, _, Status, _, _)
    , !  % just one is necessary
.


% Normally the PRType can be compared directly, but as a special case,
% a configuration identified as PR_Solo for one project could also be
% involved in another project where that PR affects multiple repos and
% therefore be a PR_Repogrouped, so allow those two to equate to each
% other.  This function is successful if two PRTypes are equivalent
% under the above rules and returns the pre-eminent PRType to use.
cmpPrType(pr_type(pr_solo,_P,R,I), PT1, PT1) :-
    PT1 = pr_type(pr_repogroup,_P2,I,RL)
    , PT1
    % , pr_type(pr_solo,R,I)
    , member(R, RL)
    , !
.
cmpPrType(PT1, pr_type(pr_solo,P,R,I), PT1) :-
    PT1 = pr_type(pr_repogroup,_P2,I,RL)
    , PT1
    , pr_type(pr_solo,P,R,I)
    , member(R, RL)
    , !
.
cmpPrType(PT1, PT1, PT1).


cmpPRCfg(PC1, PC2) :- listcmp(PC1, PC2).


branch_for_prtype(pr_type(pr_solo, _, Repo, _), Branch) :-
    is_main_branch(Repo, Branch)
.
branch_for_prtype(pr_type(pr_repogroup, _, _, RepoList), Branch) :-
    nth0(0, RepoList, R0)
    , is_main_branch(R0, Branch)
.
branch_for_prtype(pr_type(pr_grouped, Branch), Branch).

prcfg_has_user([prcfg(_Repo, _PRID, _Branch, _BRType, User, _Email)|_], User).
prcfg_has_user([_|PRCfgs], User) :- prcfg_has_user(PRCfgs, User).


repo_in_prcfg(Repo, [prcfg(Repo, _, _, _, _, _)|_]) :- !.
repo_in_prcfg(Repo, [_|Rems]) :- repo_in_prcfg(Repo, Rems).


