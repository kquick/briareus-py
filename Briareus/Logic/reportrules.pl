%% input facts are VCS facts plus build results:
%%
%% bldres(ProjRepo, branchtype, "branchname", strategy, [varvalue("ProjRepo", "vname", "value"), ...], builderCfgName, njobs, nsucceeded, nfailed, nscheduled, cfgstatus)
%%   cfgstatus = configValid | configError
%%
%% The rules here form the first layer of results: reporting based on build configurations and results

good_status(succeeded).
good_status(initial_success).
good_status(fixed).

bad_status(failed).
bad_status(badconfig).

listcmp([A|AS], BS) :- member(A, BS), listcmp(AS, BS).
listcmp([], _).

report(status_report(succeeded, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, Vars)) :-
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(ProjRepo, BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    prior_status(Status, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, PriorVars),
    good_status(Status),
    listcmp(Vars, PriorVars).

report(status_report(fixed, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, Vars)) :-
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(ProjRepo, BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    prior_status(failed, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, PriorVars),
    listcmp(Vars, PriorVars).

report(status_report(initial_success, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, Vars)) :-
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(ProjRepo, BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    findall(S, (prior_status(S, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, PriorVars),
                listcmp(Vars, PriorVars)), PS),
    length(PS, 0).

report(status_report(failed, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, Vars)) :-
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(ProjRepo, BranchType, Branch, Strategy, Vars, Bldcfg, _, _, N, 0, configValid),
    N > 0.

report(status_report(badconfig, project(ProjRepo), Strategy, BranchType, Branch, Bldcfg, Vars)) :-
    is_project_repo(ProjRepo),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(ProjRepo, BranchType, Branch, Strategy, Vars, Bldcfg, _, _, _, _, configError).

report(complete_failure(ProjRepo)) :-
    is_project_repo(ProjRepo),
    findall(X, (report(status_report(S,project(ProjRepo),_,_,_,X,_)),
                good_status(S)),
            Res),
    length(Res, 0).

report(var_failure(ProjRepo, N, V)) :-
    varvalue(ProjRepo, N, V),
    findall(X, (report(status_report(S,project(ProjRepo),_,_,_,X,Vars)),
                good_status(S),
                member(varvalue(ProjRepo, N, V), Vars)),
            Res),
    length(Res, 0),
    \+ report(complete_failure(ProjRepo)).

report(config_error(ProjRepo, Cfg)) :-
    is_project_repo(ProjRepo),
    bldres(ProjRepo, _BranchType, _Branch, _Strategy, _Vars, Cfg, _, _, _, _, configError).

%% ------------------------------------------------------------
%% PR assessments

report(pr_success(Branch, RIS)) :-
    branch_type(pullreq, Branch),
    pr_failures(Branch, RIS, Cfgs),
    length(Cfgs, 0).

report(pr_failure(Branch, RIS)) :-
    pr_failures(Branch, RIS, Cfgs),
    length(Cfgs, N), N > 0.

report(pr_failing(ProjRepo, Branch, "strategy-TBD", Cfgs)) :-
    is_project_repo(ProjRepo),
    branch_type(pullreq, Branch),
    findall(X, (report(status_report(S,project(ProjRepo),_,pullreq,Branch,X,_)),
                bad_status(S)),
            Cfgs).

pr_failures(Branch, RIS, Cfgs) :-
    branch_type(pullreq, Branch),
    findall(X, (is_project_repo(ProjRepo),
                report(status_report(S,project(ProjRepo),_,pullreq,Branch,X,_)),
                bad_status(S)),
            Cfgs),
    findall((R,I), pullreq(R,I,Branch), RIS).
