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
%% The rules here form the first layer of results: reporting based on build configurations and results

good_status(succeeded).
good_status(initial_success).
good_status(fixed).

bad_status(failed).
bad_status(N) :- integer(N).
bad_status(badconfig).

listcmp([A|AS], BS) :- member(A, BS), listcmp(AS, BS).
listcmp([], _).

% Normally the BldDesc can be compared directly, but as a special
% case, a configuration identified as PR_Solo for one project could
% also be involved in another project where that PR affects multiple
% repos and therefore be a PR_Repogrouped, so allow those two to
% equate to each other.  This function compares the BldDescs and
% returns the pre-eminent BldDesc to use.
cmpBldDesc(D1, D1, D1).
cmpBldDesc(pr_type(pr_solo,R,I),
           pr_type(pr_repogroup,I,RL), pr_type(pr_repogroup,I,RL)) :-
    member(R, RL).
cmpBldDesc(pr_type(pr_repogroup,I,RL),
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
    , findall(BName,
              (bldres(PName, BranchType, Branch, Strategy, Vars2, BName, _, _, _, _, _, BldDesc2)
               , cmpBldDesc(BldDesc, BldDesc2, _)
               , listcmp(Vars1, Vars2))
              , BNames)
    , length(BNames, 0)
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
    project(PName, _),
    findall(X, (report(status_report(S,project(PName),_,_,_,X,_,_)),
                good_status(S)),
            Res),
    length(Res, 0).

report(var_failure(PName, N, V)) :-
    varvalue(PName, N, V),
    findall(X, (report(status_report(S,project(PName),_,_,_,X,Vars,_)),
                good_status(S),
                member(varvalue(PName, N, V), Vars)),
            Res),
    length(Res, 0),
    \+ report(complete_failure(PName)).


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
                 , findall(N2
                           , (bldres(Project, pullreq, Branch, Strategy, Vars2, N2, _, _, _, _, _, BldDesc2)
                              , cmpBldDesc(BldDesc, BldDesc2, _)
                              , listcmp(Vars1, Vars2)),
                           N2s)
                 , length(N2s, 0)
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
