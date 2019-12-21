# Support for various backend builders

def buildcfg_name(bldcfg):
    fix_branchname = lambda bn: bn.replace('/', '~~')
    if bldcfg.bldvars:
        vnames = sorted([ v.varname for v in bldcfg.bldvars ])
        vdict = dict( [(v.varname, v.varvalue) for v in bldcfg.bldvars] )
        varparts = [ vdict[n] for n in vnames ]
    else:
        varparts = []
    parts = "-".join([".".join([ fix_branchname(bldcfg.branchname),
                                 bldcfg.strategy])] + varparts)
    if bldcfg.branchtype == "pullreq":
        # n.b. a PR jobset is not identified by a single PR number
        # (pr_ident) because the PR number is repo-specific and
        # it's possible that the PR exists for multiple repos.
        #
        # Any PR's that share the same branch name are assumed to
        # be related and built together, so there may be many
        # PR's.  The assumed workflow is that a PR in one repo
        # will cause build failures in a downstream repo, which
        # will be addressed by creating an identically-named
        # branch in the downstream repo for the fixes and
        # eventually turning that into a PR in that repo as well.
        #
        # The exception is the "master" branch, because it's a
        # common occurrence for a developer to fork master, make a
        # change, and submit a PR for that fork without switching
        # to a branch in the fork first; there's no expectation
        # that these types of PR's are intended to be coordinated.
        #
        # Unfortunately, for both the "master" branch pull
        # request, and the case where the *same* repo has multiple
        # pull requests that have the same name in both source
        # fork repos, it's not possible to uniquely identify one
        # of these with simply the branch name and knowledge that
        # it is a PR.
        #
        # The jobname for PR-based builds is therefore composed of
        # the branch name and all the PR numbers.  The unfortunate
        # part of this is that if a new PR is created in a related
        # repo with the same branch name (see the workflow
        # described above), the jobname will change, losing
        # history continuity for tracking progress on the PR.
        prnums = sorted(list(set([ "PR" + brr.pullreq_id
                                   for brr in bldcfg.blds
                                   if brr.pullreq_id != "project_primary"
        ])))
        return '-'.join(prnums +
                        ['.'.join([fix_branchname(bldcfg.branchname),
                                   bldcfg.strategy])] +
                        varparts)
    return '-'.join(['.'.join([fix_branchname(bldcfg.branchname),
                               bldcfg.strategy])] +
                    varparts)
