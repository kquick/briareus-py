%% Input facts are VCS facts plus build results (generated by the Builder):
%%
%% bldres(PName, branchtype, "branchname", strategy,
%%        [varvalue("PName", "vname", "value"), ...],
%%        builderCfgName, njobs, nsucceeded, nfailed, nscheduled, cfgstatus,
%%        description)
%%
%%   branchtype = pullreq | regular
%%   strategy   = standard | heads | submodules
%%   cfgstatus  = configValid | configError
%%   description = MainBranch(ProjRepo, Branch)
%%                 | BranchReq(PName, Branch)
%%                 | PR_Solo(Repo, PRNum)
%%                 | PR_Repogroup(PRNum, RepoList)
%%                 | PR_Grouped(Branch)
%%
%% The rules here form the first layer of results: reporting based on
%% build configurations and results.  Some additional notes:
%%
%%  * Any configuration errors are treated differently that build
%%    failures.  A configuration error represents an issue with the
%%    build configuration and is corrected by the team managing the
%%    build configuration, which may be different than the team
%%    managing the code itself.
%%
%%  * There may be build configurations that have not yet been
%%    processed by the builder, so the collection of results from the
%%    builder did not obtain bldres reports for those.  The
%%    missing_bldres rule will synthesize a "pending" bldres for
%%    these, although neither the job count nor the BldName is yet
%%    known.

good_status(succeeded).
good_status(initial_success).
good_status(fixed).

bad_status(failed).
bad_status(N) :- integer(N).
bad_status(badconfig).

listcmp([A|AS], BS) :- member(A, BS), listcmp(AS, BS).
listcmp([], _).

%% ----------------------------------------------------------------------

%% no_good_status returns true if the project has no
%% validly-configured builds that are finished with a good status
%% result.
no_good_status(PName) :-
    bldres(PName, _, _, _, _, _, N, N, 0, 0, configValid, _), !, fail.
no_good_status(_PName).

%% no_pending_status returns true if the project has no pending
%% validly-configured builds.
no_pending_status(PName) :-
    bldres(PName, _, _, _, _, _, _, _, _, N, configValid, _)
    , N > 0
    , !, fail
.
no_pending_status(PName) :-
    missing_bldres(bldres(PName, _, _, _, _, _, _, _, _, _, configValid, _)), !, fail.
no_pending_status(_PName).

%% no_badconfig returns true if there are no configuration errors for
%% the project.
no_badconfig(PName) :-
    bldres(PName, _, _, _, _, _, _, _, _, _, configError, _), !, fail.
no_badconfig(_PName).

%% ----------------------------------------------------------------------

%% no_good_varvalue_status returns true if the project has no
%% validly-configured builds using the specified variable value that
%% are finished with a good status result.
no_good_varvalue_status(PName, VarName, VarValue) :-
    bldres(PName, _, _, _, Vars, _, N, N, 0, 0, configValid, _)
    , member(varvalue(PName, VarName, VarValue), Vars)
    , !
    , fail
.
no_good_varvalue_status(_PName, _VarName, _VarValue).


%% no_pending_status returns true if the project has no pending
%% validly-configured builds.
no_pending_varvalue_status(PName, VarName, VarValue) :-
    bldres(PName, _, _, _, Vars, _, _, _, _, N, configValid, _)
    , N > 0
    , member(varvalue(PName, VarName, VarValue), Vars)
    , !
    , fail
.
no_pending_varvalue_status(PName, VarName, VarValue) :-
    missing_bldres(bldres(PName, _, _, _, Vars, _, _, _, _, _, configValid, _))
    , member(varvalue(PName, VarName, VarValue), Vars)
    , !
    , fail
.
no_pending_varvalue_status(_PName, _VarName, _VarValue).

%% no_badconfig returns true if there are no configuration errors for
%% the project.
no_varvalue_badconfig(PName, VarName, VarValue) :-
    bldres(PName, _, _, _, Vars, _, _, _, _, _, configError, _)
    , member(varvalue(PName, VarName, VarValue), Vars)
    , !
    , fail
.
no_varvalue_badconfig(_PName, _VarName, _VarValue).

%% ----------------------------------------------------------------------

%% missing_bldres will synthesize a "pending" bldres for any bldcfg
%% that does not have a builder-reported bldres (the assumption is
%% that the builder hasn't seen the job yet).
missing_bldres(bldres(PName, BrType, Branch, Strategy, Vars, BldName, 1, 0, 0, 1, configValid, BldDesc)) :-
    build_config2(bldcfg(PName, BrType, Branch, Strategy, BldDesc, _Blds, Vars))
    , no_bldres(PName, BrType, Branch, Strategy, BldDesc, Vars)
    , BldName = "TBD"
.

%% no_bldres is a red-cut cut-fail check for a missing bldres given
%% bldcfg parameters it should match.  This is used by missing_bldres
%% (which is probably the better rule to use).
no_bldres(PName, BrType, Branch, Strategy, BldDesc, Vars) :-
    bldres(PName, BrType, Branch, Strategy, Vars2, _, _, _, _, _, _, BldDesc2)
    , cmpBldDesc(BldDesc, BldDesc2, _)
    , listcmp(Vars, Vars2)
    , !
    , fail
.
no_bldres(_, _, _, _, _, _).


% Normally the PRType or corresponding BldDesc can be compared
% directly, but as a special case, a configuration identified as
% PR_Solo for one project could also be involved in another project
% where that PR affects multiple repos and therefore be a
% PR_Repogrouped, so allow those two to equate to each other.  This
% function compares the BldDescs and returns the pre-eminent BldDesc
% to use.
cmpBldDesc(D1, D1, D1) :- ! .  % red cut to avoid duplicates on below
cmpBldDesc(D1, D2, D3) :- cmpPrType(D1, D2, D3).

cmpPrType(PT1, PT1, PT1).
cmpPrType(pr_type(pr_solo,R,I),
          pr_type(pr_repogroup,I,RL), pr_type(pr_repogroup,I,RL)) :-
    member(R, RL).
cmpPrType(pr_type(pr_repogroup,I,RL),
          pr_type(pr_solo,R,I), pr_type(pr_repogroup,I,RL)) :-
    member(R, RL).



report(status_report(succeeded, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, N, N, 0, 0, configValid, BldDesc1),
    prior_status(Status, project(PName), Strategy, BranchType, Branch, Bldname, PriorVars, BldDesc2),
    good_status(Status),
    cmpBldDesc(BldDesc1, BldDesc2, BldDesc),
    listcmp(Vars, PriorVars).

report(status_report(fixed, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, N, N, 0, 0, configValid, BldDesc1),
    prior_status(PrevSts, project(PName), Strategy, BranchType, Branch, Bldname, PriorVars, BldDesc2),
    bad_status(PrevSts),
    cmpBldDesc(BldDesc1, BldDesc2, BldDesc),
    listcmp(Vars, PriorVars).

report(status_report(initial_success, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, N, N, 0, 0, configValid, BldDesc),
    findall(S, (prior_status(S, project(PName), Strategy, BranchType, Branch, Bldname, PriorVars, BldDesc2),
                cmpBldDesc(BldDesc, BldDesc2, _),
                listcmp(Vars, PriorVars)), PS),
    length(PS, 0).

report(status_report(N, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, _, _, N, 0, configValid, BldDesc),
    N > 0.

report(status_report(badconfig, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, _, _, _, _, configError, BldDesc).

% Note, pending_status is different than status_report because
% status_report wants to track transitions (fixed v.s. initial vis
% still good) and with only one layer of history, introducing a
% status_report(pending, ...) would obscure the previous results.
report(pending_status(project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _),
    strategy(Strategy, PName, Branch),
    branch_type(BranchType, Branch, _),
    bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, _, _, _, N, configValid, BldDesc),
    N > 0.


report(new_pending(bldcfg(PName, BranchType, Branch, Strategy, BldDesc, Blds, Vars1))) :-
    % configs for which there is no bldres yet (e.g. the .jobsets
    % hasn't run) There is no Bldname assigned, but Briareus can
    % synthesize one from the bldcfg.
    project(PName, _)
    , strategy(Strategy, PName, Branch)
    , branch_type(BranchType, Branch, _)
    , build_config2(bldcfg(PName, BranchType, Branch, Strategy, BldDesc, Blds, Vars1))
    , no_bldres(PName, BranchType, Branch, Strategy, BldDesc, Vars1)
    .


% This preserves the previous status for a pending build
report(status_report(Sts, project(PName), Strategy, BranchType, Branch, Bldname, Vars, BldDesc)) :-
    project(PName, _)
    , strategy(Strategy, PName, Branch)
    , branch_type(BranchType, Branch, _)
    , bldres(PName, BranchType, Branch, Strategy, Vars, Bldname, _, _, _, N, configValid, BldDesc1)
    , N > 0
    , prior_status(Sts, project(PName), Strategy, BranchType, Branch, Bldname, PriorVars, BldDesc2)
    , cmpBldDesc(BldDesc1, BldDesc2, BldDesc)
    , listcmp(Vars, PriorVars)
    .

report(complete_failure(PName)) :-
    project(PName, _)
    , no_good_status(PName)
    , no_pending_status(PName)
    , no_badconfig(PName)
.

report(var_failure(PName, N, V)) :-
    project(PName, _)
    , varvalue(PName, N, V)
    , no_good_varvalue_status(PName, N, V)
    , \+ report(complete_failure(PName))
    , no_pending_varvalue_status(PName, N, V)
    , no_varvalue_badconfig(PName, N, V)
.


%% ------------------------------------------------------------
%% PR assessments

report(pr_status(PRType, Branch, Project, PRCfg, GoodBlds, BadBlds, PendingBlds, NumNotStarted)) :-
    % Return once for each PRType + ProjRepo, providing status of all Blds for that PRType + ProjRepo
    pr_config(PRType, Project, PRCfg)
    , branch_for_prtype(PRType, Branch)
    , project(Project, _ProjRepo)
    , findall(BldName
              , (bldres(Project, pullreq, Branch, _, _, BldName, _, _, _, N, configValid, BldDesc)
                 , cmpBldDesc(PRType, BldDesc, _)
                 , N > 0
              )
              , PendingBlds)
    , findall(BldDesc
              , (build_config2(bldcfg(Project, pullreq, Branch, Strategy, BldDesc, _, Vars1))
                , no_bldres(Project, pullreq, Branch, Strategy, BldDesc, Vars1)
              )
              , BDS)
    , length(BDS, NumNotStarted)
    , findall(BldName2
              , ((bldres(Project, pullreq, Branch, _, _, BldName2, _, _, M, 0, configValid, BldDesc2)
                  , cmpBldDesc(PRType, BldDesc2, _)
                  , M > 0)
                ; bldres(Project, pullreq, Branch, _, _, BldName2, _, _, _, _, configError, BldDesc3)
                  , cmpBldDesc(PRType, BldDesc3, _)
              )
              , BadBlds)
    , findall(BldName3
              , (bldres(Project, pullreq, Branch, _, _, BldName3, Z, Z, 0, 0, configValid, BldDesc4)
                 , cmpBldDesc(PRType, BldDesc4, _)
                )
              , GoodBlds)
.
