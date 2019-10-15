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

        # Specifies the existence of a submodule specification.  The
        # format is: submodule(ProjectRepo, BranchName, SubmoduleRepo,
        # SubmoduleRef).  The SubmoduleRef is the explicit VCS
        # reference that the submodule refers to.
        DeclareFact('submodule/4'),

        # Specifies a variable that the user has declared to be of
        # interest.  Variables names are identified separately from
        # values to allow the variables to be easily enumerated.
        DeclareFact('varname/1'),

        # Specifies one possible value for a specified variable, as
        # obtained from the user's input specification.
        DeclareFact('var/2'),
    ]

    repo_facts    = [ Fact('repo("%s")'    % r.repo_name)   for r in RL ]
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

    repobranch_facts = [ Fact('branch("%s", "%s")' % rb)
                         for rb in repo_info['branches'] ]

    submodules_facts = []
    if project:
        pn = project.repo_name
        # n.b. repo_info['subrepos'] are of type SubRepoInfo from InternalOps;
        # the actual definition is not imported here because Python is
        # duck-typed.
        submods_data = lambda bname: [ (e.sm_sub_name, e.sm_sub_vers)
                                       for e in repo_info['submodules']
                                       if e.sm_repo_name == pn and e.sm_branch == bname ]
        for b in BL:
            bn = b.branch_name
            for repover in submods_data(bn):
                submodules_facts.append( Fact('submodule("%s", "%s", "%%s", "%%s")' % (pn, bn) % repover) )
        for p in pullreqs:
            if p.pr_target_repo == project.repo_name:
                for repover in submods_data(p.pr_branch):
                    submodules_facts.append( Fact('submodule("%(pr_target_repo)s", "%(pr_branch)s", "%%s", "%%s")' % p.__dict__ % repover) )

    varname_facts = []
    varval_facts = []
    for var in VAR:
        varname_facts.append( Fact('varname("%s")' % var.variable_name) )
        varval_facts.extend( [ Fact('var("%s", "%s")' % (var.variable_name, val))
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
