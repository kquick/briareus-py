from Briareus.Logic.Evaluation import DeclareFact, Fact


def get_input_facts(RL, BL, VAR, repo_info):

    if not RL:
        return []  # dummy run, build nothing

    projects = [ r for r in RL if r.project_repo ]

    if len(projects) > 1:
        raise AssertionError("only one 'project' allowed in input specification.")
    project = projects[0] if projects else None

    declare_facts = [ DeclareFact('repo/1'),
                      DeclareFact('project/1'),
                      DeclareFact('branchreq/2'),
                      DeclareFact('subrepo/1'),
                      DeclareFact('pullreq/3'),
                      DeclareFact('branch/2'),
                      DeclareFact('submodule/4'),
                      DeclareFact('varname/1'),
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
