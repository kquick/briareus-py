%% input facts are VCS facts plus build results:
%%
%% bldres(branchtype, "branchname", strategy, [var("varname", "varvalue"), ...], builderCfgName, njobs, nsucceeded, nfailed, nscheduled, cfgstatus)
%%   cfgstatus = configValid | configError

report(status_report(succeeded, project(R), ProjRepo, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, N, N, 0, 0, configValid).
report(status_report(failed, project(R), ProjRepo, Bldcfg, Vars)) :-
    is_project_repo(R),
    strategy(Strategy, ProjRepo, Branch),
    branch_type(BranchType, Branch),
    bldres(BranchType, Branch, Strategy, Vars, Bldcfg, _, _, N, 0, configValid), N > 0.                                  
