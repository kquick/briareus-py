from Briareus.Logic.Evaluation import DeclareFact, Fact


def get_input_facts(RL, BL, VAR, repo_info):

    if not RL:
        return []  # dummy run, build nothing

    projects = [ r for r in RL if r.project_repo ]

    if len(projects) > 1:
        raise AssertionError("only one 'project' allowed in input specification.")
    project = projects[0] if projects else None

    declare_facts = [

        # Identifies a repository (by name, not URL, so
        # forks are all assumed to be identical
        # repositories.)
        DeclareFact('repo/1'),

        # Identifies the repo that is the "Project"
        # repo.  The "Project" repo is the only one
        # where submodules are looked up, and the
        # "Project" also serves as the "top-level"
        # identification of the group of repositories
        # (there may be multiple Projects under
        # consideration).
        DeclareFact('project/1'),

        # Specifies the name of the main branch for the associated
        # repository.  The first argument is the Project repo and the
        # second argument is the name of the main branch.  This is
        # used to handle cases where the main branch is not the
        # expected one (e.g. "master" or "trunk", specified by the
        # default_main_branch fact).
        DeclareFact('main_branch/2'),

        # Specifies a branch that the user would like to have built.
        # The branch request is associated with the Project
        # specification from which it came: branchreq(ProjectRepo,
        # BranchName)
        DeclareFact('branchreq/2'),

        # Identifies a repository found by checking the submodules
        # specification of the Project repo; this repository was not
        # explicitly identified by the user.  The only real difference
        # is that identified repositories may be changed in different
        # branches of the Project repo, but primary repositories are
        # always considered for all build configurations.
        DeclareFact('subrepo/1'),

        # Identifies a pull request that was found by probing the VCS.
        # The format is: pullreq(Repo, PR_Ident, PR_Branch).  The
        # PR_Branch may be used to correlate against branches in other
        # repositories (whether or not they have an active pullreq for
        # that branch).  The PR_Ident serves only to identify this PR
        # (relative to the Repo) and is otherwise "free-form".
        DeclareFact('pullreq/3'),

        # Specifies the existence of a branch in a repository by
        # probing the VCS.  The format is: branch(Repo, BranchName).
        DeclareFact('branch/2'),

        # Specifies the existence of a submodule specification; this
        # corresponds to the SubModuleInfo as defined in
        # Briareus/VCS/InternalMessages.py.  The format is:
        # submodule(ProjectRepo, PullReqID, BranchName, SubmoduleRepo,
        # SubmoduleRef).  The SubmoduleRef is the explicit VCS
        # reference that the submodule refers to.  The PullReqID is
        # project_primary (an atom as opposed to a PR ID string) if this
        # reference is for the project repo and not a pull request.
        DeclareFact('submodule/5'),

        # Specifies a variable that the user has declared to be of
        # interest.  Variables names are identified separately from
        # values to allow the variables to be easily enumerated.
        # Format is: varname(ProjectRepo, VarName)
        DeclareFact('varname/2'),

        # Specifies one possible value for a specified variable, as
        # obtained from the user's input specification.  Format is:
        # varvalue(ProjectRepo, VarName, VarValue)
        DeclareFact('varvalue/3'),
    ]

    repo_facts    = ([ Fact('default_main_branch("master")') ] +
                     # ^^ note: for a multi-project config, possibly
                     # involving other repos, this isn't necessarily
                     # globally true.  When this varied support is
                     # necesary, this fact might need a projectname
                     # addition.
                     [ Fact('repo("%s")'    % r.repo_name)   for r in RL ] +
                     [ Fact('main_branch("%s", "%s")' % (r.repo_name, r.main_branch))
                       for r in RL if r.main_branch != "master" ])
    project_facts = [ Fact('project("%s")' % r.repo_name)   for r in projects ]
    branch_facts  = [ Fact('branchreq("%s", "%s")'  % (r.repo_name, b.branch_name))
                      for r in projects for b in BL ]
    subrepo_facts = [ Fact('subrepo("%s")' % r.repo_name)
                      for r in repo_info['subrepos'] ]

    # n.b. repo_info['pullreqs'] are of type PRInfo from InternalOps;
    # the actual definition is not imported here because Python is
    # duck-typed.
    pullreqs = repo_info['pullreqs']
    pullreq_facts = [ Fact('pullreq("%(pr_target_repo)s", "%(pr_ident)s", "%(pr_branch)s")' %
                           p.__dict__)
                      for p in pullreqs ]

    # n.b. See note in InternalOps: a pullreq for a repo
    # "overrides" a similarly-named branch for that repo, so in
    # general a pullreq will suppress a branch declaration, but it
    # will cause a check on all other repositories (including
    # other projects sharing this repository) for the branch.

    repos_without_branches = [r.repo_name for r in RL] + [r.repo_name for r in repo_info['subrepos']]
    for rb in repo_info['branches']:
        while rb[0] in repos_without_branches:
            repos_without_branches.remove(rb[0])
    if repos_without_branches:
        raise RuntimeError("The following repos have no available branches: %s"
                           % str(repos_without_branches))

    repobranch_facts = [ Fact('branch("%s", "%s")' % rb)
                         for rb in repo_info['branches'] ]

    submodules_facts = []
    if project:
        pn = project.repo_name
        # n.b. repo_info['submodules'] are of type SubModuleInfo from InternalOps;
        # the actual definition is not imported here because Python is
        # duck-typed.
        submods_data = lambda bname, pr_id: [ (e.sm_sub_name, e.sm_sub_vers)
                                              for e in repo_info['submodules']
                                              if (e.sm_repo_name == pn
                                                  and e.sm_branch == bname
                                                  and e.sm_pullreq_id == pr_id
                                                 )]
        for bn in set([b.branch_name for b in BL] + [project.main_branch]):
            for repover in submods_data(bn, None):
                submodules_facts.append( Fact('submodule("%s", project_primary, "%s", "%%s", "%%s")'
                                              % (pn, bn) % repover) )
        for p in pullreqs:
            if p.pr_target_repo == project.repo_name:
                for repover in submods_data(p.pr_branch, p.pr_ident):
                    submodules_facts.append(
                        Fact('submodule("%(pr_target_repo)s", "%(pr_ident)s", "%(pr_branch)s", "%%s", "%%s")'
                             % p.__dict__ % repover) )

    varname_facts = []
    varval_facts = []
    for r in projects:
        for var in VAR:
            varname_facts.append( Fact('varname("%s", "%s")' % (r.repo_name, var.variable_name)) )
            varval_facts.extend( [ Fact('varvalue("%s", "%s", "%s")' %
                                        (r.repo_name, var.variable_name, val))
                                   for val in var.variable_values ] )

    return (declare_facts +
            project_facts +
            repo_facts +
            subrepo_facts +
            branch_facts +
            repobranch_facts +
            pullreq_facts +
            submodules_facts +
            varname_facts +
            varval_facts
            )
