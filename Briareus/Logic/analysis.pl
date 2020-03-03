%% This is a logic layer that uses the VCS facts, the buildcfg rules, the
%% build results, and the reportrules to perform a higher-level
%% analysis of the status.

:- discontiguous analysis/1.
                 
%% handle the build failures for this variable separately from other
%% analyses if the variable has completely failed and there are other
%% values for the variable that have not completely failed.
analysis(var_handled_separately(Project, VarName, VarValue)) :-
    report(var_failure(Project, VarName, VarValue)),
    findall(V, (varvalue(Project, VarName, V)), VS),
    delete(VS, VarValue, VVS),
    length(VVS, L),
    L > 0,
    other_var_failures(Project, VarName, VVS, FS),
    length(FS, LF),
    LF < L.

other_var_failures(_Project, _VarName, [], []).
other_var_failures(Project, VarName, [V|VS], Res) :-
    report(var_failure(Project, VarName, V)),
    other_var_failures(Project, VarName, VS, RSub),
    cons(V, RSub, Res).
other_var_failures(Project, VarName, [V|VS], Res) :-
    other_var_failures(Project, VarName, VS, Res),
    \+ report(var_failure(Project, VarName, V)).

cons(H, T, [H|T]).
% identity(V,V).

%% ----------------------------------------------------------------------
%% Various helpers, frequently a cut-fail pattern.

no_pending_branch(PName, Branch) :-
    bldres(PName, regular, Branch, _, _, _, _, _, _, N, configValid, _)
    , N > 0
    , !
    , fail
.
no_pending_branch(PName, Branch) :-
    missing_bldres(bldres(PName, regular, Branch, _, _, _, _, _, _, _, configValid, _))
    , !
    , fail
.
no_pending_branch(_PName, _Branch).

no_failing_branch(PName, Branch) :-
    bldres(PName, regular, Branch, _, _, _, _, N, _, _, configValid, _)
    , N > 0
    , !
    , fail
.
no_failing_branch(_PName, _Branch).

no_branch_badconfig(PName, Branch) :-
    bldres(PName, regular, Branch, _, _, _, _, _, _, _, configError, _)
    , !
    , fail
.
no_branch_badconfig(_PName, _Branch).

not_a_var_failure(Project, Vars) :-
    member(varvalue(Project, N, V), Vars)
    , report(var_failure(Project, N, V))
    , !
    , fail
.
not_a_var_failure(_P, _V).

no_failing_non_varfailure_branch(PName, Branch) :-
    bldres(PName, regular, Branch, _, Vars, _, _, _, N, _, configValid, _)
    , N > 0
    , not_a_var_failure(PName, Vars)
    , !
    , fail
.
no_failing_non_varfailure_branch(_PName, _Branch).

at_least_one_success(PName, Branch) :-
    bldres(PName, regular, Branch, _, _, _, N, N, 0, 0, configValid, _)
    , !
.

%% ----------------------------------------------------------------------
%% Notifications
%%
%% The most common Action is a notify(What, Subject, Parameters) which
%% identifies What the notification is about, the Subject of the
%% notification (a Project, a Branch, a Pull Request, etc.), and the
%% Parameters which provide the details about that notification.
%% What the Parameters are is determined by the What + Subject.


action(notify(completely_broken, Project, NBldCfgs)) :-
    report(complete_failure(Project))
    , findall(C, bldres(Project, _, _, _, _, C, _, _, _, _, _, _), Cfgs)
    , length(Cfgs, NBldCfgs)
    , NBldCfgs > 0
.

action(notify(variable_failing, Project, varvalue(Project, VarName, VarValue))) :-
    project(Project, _)
    , report(var_failure(Project, VarName, VarValue))
    , analysis(var_handled_separately(Project, VarName, VarValue)).

%% generate a notification if the master_submodules is broken (the
%% most important build), ignoring any variables that are completely
%% failing.

action(notify(main_submodules_broken, Project, Configs)) :-
    project(Project, R)
    , is_main_branch(R, MainBr)
    , useable_submodules(Project, R, MainBr)
    , \+ report(complete_failure(Project))
    , no_pending_branch(Project, MainBr)
    , no_branch_badconfig(Project, MainBr)
    , findall(C
              , (bldres(Project, regular, MainBr, submodules, Vars, C, _, _, N, _, configValid, _)
                , N > 0
                 , not_a_var_failure(Project, Vars)
              )
              , CS)
    , length(CS, N), N > 0
    , sort(CS, Configs)
.

action(notify(main_submodules_good, Project, MainBr)) :-
    project(Project, R)
    , is_main_branch(R, MainBr)
    , useable_submodules(Project, R, MainBr)
    , \+ report(complete_failure(Project))
    , no_pending_branch(Project, MainBr)
    , no_failing_non_varfailure_branch(Project, MainBr)
    , no_branch_badconfig(Project, MainBr)
    , at_least_one_success(Project, MainBr)
.

action(notify(main_good, Project, MainBr)) :-
    project(Project, R)
    , is_main_branch(R, MainBr)
    , \+ report(complete_failure(Project))
    % No submodules builds; if there were, this would be a main_submodules_good
    , \+ useable_submodules(Project, R, MainBr)
    , no_pending_branch(Project, MainBr)
    , no_failing_non_varfailure_branch(Project, MainBr)
    , no_branch_badconfig(Project, MainBr)
    , at_least_one_success(Project, MainBr)
.

action(notify(main_broken, Project, CS)) :-
    project(Project, R)
    , is_main_branch(R, MainBr)
    , \+ report(complete_failure(Project))
    , \+ useable_submodules(Project, R, MainBr)
    , no_pending_branch(Project, MainBr)
    , no_branch_badconfig(Project, MainBr)
    , findall(C
              , (bldres(Project, _, _, standard, Vars, C, _, _, N, _, configValid, _)
                 , N > 0
                 , not_a_var_failure(Project, Vars)
              )
              , CS)
    , length(CS, CSN)
    , CSN > 0
.

% ----------------------------------------------------------------------

% For a particular PR, it can be useful to know on a
% Project-by-Project basis what the status of that PR is.  One example
% usage of this is posting forge_status notifications (i.e. the CI
% status indicators on a PR): these status notifications are limited
% to 140 characters and builds for that PR may differ across various
% projects, so a success/failure status posting to the PR for each
% individual project is the best route and can be indicated by using
% these actions.
%
% The Subject is the Project for which the status should be set on the
% PR.
%
% The PR location(s) are obtained from the PRCfg.
action(notify(pr_projstatus_pending, Project, prdata(PRType, PRCfg))) :-
    report(pr_status(PRType, _Branch, Project, PRCfg, _, _, Pends, NumToStart))
    , length(Pends, NPend)
    , (NPend > 0 ; NumToStart > 0)
.

action(notify(pr_projstatus_good, Project, prdata(PRType, PRCfg))) :-
    report(pr_status(PRType, _Branch, Project, PRCfg, Goods, [], [], 0))
    , length(Goods, NGood)
    , NGood > 0
.

action(notify(pr_projstatus_fail, Project, prfaildata(PRType, PRCfg, Goods, Fails))) :-
    report(pr_status(PRType, _Branch, Project, PRCfg, Goods, Fails, [], 0))
    , length(Fails, NFail)
    , NFail > 0
.

% Filter notify(pr_projstatus_X, ...) to the specified PR.
notify_is_for_PR(notify(_, _, prdata(PRType, _)), PRType).
notify_is_for_PR(notify(_, _, prfaildata(PRType, _, _, _)), PRType).

% Filter notify(any, ...) for association with the specified users
notify_is_for_user(notify(_, _, prdata(_, PRCfgs)), User) :-
    prcfg_has_user(PRCfgs, User)
    , !  % one is enough
.
notify_is_for_user(notify(_, _, prfaildata(_, PRCfgs, _, _)), User) :-
    prcfg_has_user(PRCfgs, User)
    , !  % one is enough
.


% These are the notify What status that can be reported to the forge
valid_forge_status(pr_projstatus_pending).
valid_forge_status(pr_projstatus_good).
valid_forge_status(pr_projstatus_fail).

% Filter the AllowedRepos by which ones are mentioned in the PRCfg.
forge_status_repos(AllowedRepos, prdata(_, PRCfg), TargetRepos) :-
    findall(TargetRepo
            , (member(TargetRepo, AllowedRepos)
               , repo_in_prcfg(TargetRepo, PRCfg)
            )
            , TargetRepos)
.
forge_status_repos(AllowedRepos, prfaildata(_, PRCfg, _, _), TargetRepos) :-
    findall(TargetRepo
            , (member(TargetRepo, AllowedRepos)
               , repo_in_prcfg(TargetRepo, PRCfg)
            )
            , TargetRepos)
.


%% ----------------------------------------------------------------------
%% Action bindings

email_address_useable(Addr) :-
    split_string(Addr, "@", " ", [_User|[Domain|[]]]),
    findall(D, email_domain_whitelist(D), WL_Domains),
    (length(WL_Domains, 0); member(Domain, WL_Domains)),
    \+ email_domain_blacklist(Domain),
    \+ email_user_blacklist(Addr)
.


% Determines when and for which Targets an action is enabled for,
% based on the Do operation and the Notification.  These are mapped to
% enable(DoWhat, Target, Notification) entries in the Reporting logic
% of the input configuration.
action_enabled(DoWhat, Target, Notification) :- enable(DoWhat, Target, Notification).

% Some convenience action enablers.  The project_owner(Project, Email)
% is defined in the Reporting logic of the input configuration.
action_enabled(email, UserEmail, notify(main_broken, Project, _)) :-
    project_owner(Project, UserEmail).
action_enabled(email, UserEmail, notify(main_good, Project, _)) :-
    project_owner(Project, UserEmail).
action_enabled(email, UserEmail, notify(completely_broken, Project, _)) :-
    project_owner(Project, UserEmail).
action_enabled(email, UserEmail, notify(main_submodules_broken, Project, _)) :-
    project_owner(Project, UserEmail).
action_enabled(email, UserEmail, notify(main_submodules_good, Project, _)) :-
    project_owner(Project, UserEmail).

% do_new inherits Previous from any prior specification of this type.
% Using this method, the Previous (updated by performing the
% notifications) will eventually match the Target set, at which point
% the notification processing does nothing.  When the Notification is
% no longer generated, then the do is no longer expressed and both
% previous and new versions are removed.
do_new(What, Item, Previous) :- call(What, _, Item, Previous), !.  % acquire Previous
do_new(_, _, []).  % Initially, start with no deliveries


:- discontiguous email/3.
:- discontiguous chat/3.
:- discontiguous set_forge_status/3.

do(email(Users, Notification, Notified)) :-
    Notification = notify(_, _, _),
    action(Notification),
    setof(User
          , ( action_enabled(email, User, Notification)
              , email_address_useable(User)
          )
          , Users),
    do_new(email, Notification, Notified).

do(chat(Channels, notify(What, Subject, Args), Posted)) :-
    Notification = notify(What, Subject, Args)
    , action(Notification)
    , setof(Channel, action_enabled(chat, Channel, Notification), Channels)
    , do_new(chat, Notification, Posted)
.

% Sets a status on the associated PR/MR in the forge.  The status is
% reported for the project identified as the Subject, and the
% TargetRepos are those where the PR/MR exists and the status should
% be posted.
do(set_forge_status(TargetRepos, Notification, Updated)) :-
    Notification = notify(What, Subject, CS)
    , valid_forge_status(What)
    , action(Notification)
    % Which set of target repos can the forge status be posted to
    % (i.e. there is an enable(forge_status, Repo, Notification) set).
    , setof(TargetRepo
            , (repo_in_project(Subject, TargetRepo)
               , action_enabled(forge_status, Subject, Notification)
            )
            , AllowedTargetRepos)
    % Of the postable target repos, which ones are associated with this PR?
    , forge_status_repos(AllowedTargetRepos, CS, TargetRepos)
    , length(TargetRepos, N), N > 0
    , do_new(set_forge_status, Notification, Updated)
    .
