%% input facts are VCS facts plus build results:
%%
%% bldres(ProjRepo, branchtype, "branchname", strategy, [varvalue("ProjRepo", "vname", "value"), ...], builderCfgName, njobs, nsucceeded, nfailed, nscheduled, cfgstatus)
%%   cfgstatus = configValid | configError

good_status(succeeded).
good_status(initial_success).
good_status(fixed).

listcmp([A|AS], BS) :- member(A, BS), listcmp(AS, BS).
listcmp([], _).

report(status_report(succeeded, project(R), ProjRepo, Strategy, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    prior_status(Status, project(R), ProjRepo, Strategy, Bldcfg, PriorVars),
    good_status(Status),
    listcmp(Vars, PriorVars).

report(status_report(fixed, project(R), ProjRepo, Strategy, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    prior_status(failed, project(R), ProjRepo, Strategy, Bldcfg, PriorVars),
    listcmp(Vars, PriorVars).

report(status_report(initial_success, project(R), ProjRepo, Strategy, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid),
    findall(S, (prior_status(S, project(R), ProjRepo, Strategy, Bldcfg, PriorVars),
                listcmp(Vars, PriorVars)), PS),
    length(PS, 0).

report(status_report(failed, project(R), ProjRepo, Strategy, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, _, _, N, 0, configValid),
    N > 0.

report(var_failure(V)) :-
    var_failures(VFS),
    member(V, VFS).

var_failures(Res) :-
    setof(X, (S^R^P^B^T^(report(status_report(S,P,R,T,B,[X|_])), \+ good_status(S))), Res).
