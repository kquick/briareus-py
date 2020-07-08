# Support for various backend builders

from Briareus.Types import BldConfig


def fix_jobname(branchname: str) -> str:
    """Git (and other VCS tools) allows all sorts of characters in a
       branch name (a common form often seen is "bugfix/foo"), but a
       Hydra jobset and a URL specification are much more restricted.
       This function ensures that a valid jobset and URL specification
       are generated for the branch.

       This is also used for converting variant values that are
       included in the jobname.

    """
    # Although this is a generic Builder fixup, the Hydra restriction
    # should be reasonable for most Builders, and if not then this can
    # be moved to builder-specific functionality at a future date.
    #
    # Per hydra/src/lib/Hydra/Helper/CatalystUtils.pm,
    #   projectNameRE = "(?:[A-Za-z_][A-Za-z0-9-_]*)"
    #   jobsetNameRE  = "(?:[A-Za-z_][A-Za-z0-9-_\.]*)";
    #
    # Essentially, this means that only alphanumerics, underscores,
    # dashes, and periods are allowed.
    #
    # The man page for git check-ref-format describes the rules for
    # how references are named in git.
    #
    # At present, there is no attempt to handle 100% character
    # conversion; this is an evolving fixup based on what has been
    # encountered to-date.
    #
    # Also note:
    #
    #  * A conversion from jobset name back to branch name (bijection)
    #    is convenient, but not necessary.
    #
    #  * The jobset name does not have to be a valid branch name
    #
    #  * Postgresql uses '_' as a wildcard character in some
    #    situations, and unfortunately apparently when checking for
    #    primary key uniqueness constraints.  This also seems to
    #    defeat "CASCADE ON DELETE" relations, so the underscore
    #    character is avoided.
    return branchname.replace('/', '--').replace('_','---')


def buildcfg_name(bldcfg: BldConfig) -> str:
    if bldcfg.bldvars:
        vnames = sorted([ v.varname for v in bldcfg.bldvars ])
        vdict = dict( [(v.varname, v.varvalue) for v in bldcfg.bldvars] )
        varparts = [ vdict[n] for n in vnames ]
    else:
        varparts = []
    parts = "-".join([".".join([ bldcfg.branchname, bldcfg.strategy])] +
                     varparts)

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
        # One additional consideration is that the history of a build
        # configuration is bounded by the name: if a new PR is created
        # and the job name is updated to include that PR, then that's
        # a brand new configuration (and the old configuration
        # disappears) so it's hard to determine build progress (or
        # regression) if that occurs.
        #
        # Therefore, the following semi-complicated scheme is used:
        #
        #  * Normally for a pullreq build the name is
        #    "PR-{branchname}..." and the PR number does not appear in
        #    the name (it will in the description).  This should be a
        #    stable name.
        #
        #  * If the PR is for the master branch of a repo, then it is
        #    assumed that it should be distinct from other master
        #    branch PRs so the name is "PR{N}-{branchname}...".  This
        #    should be a stable name.
        #
        #    Note that this currently doesn't have information about
        #    repos whose main branch name is not "master", but
        #    multiple PR's for those will be handled by the next case.
        #
        #    Also note that while this case appears to be covered by
        #    the following case, the difference is in the stability of
        #    the naming for this case, thus this case is preferred to
        #    the following case.
        #
        #  * If there are multiple PR's for the same repo with the
        #    same branch name, these are assumed to be distinct
        #    relative to this repo, but not to others, so the name is
        #    "PR{N}-{branchname}" (as with the above case).  This is
        #    not a stable name: it may start as "PR-{branchname}" (the
        #    normal case) until the second PR is created, at which
        #    time it will change to include the number (this case),
        #    but then when one of the PR's is merged and it is back to
        #    a single PR, it will return to the normal naming.  This
        #    is assumed to be an exception case, so there is currently
        #    no protection against this; handling this would require a
        #    persistent registry of PRs.

        # The bldcfg.description for strategy=pullreq is the pr_type.
        prinfo = "PR" + getattr(bldcfg.description, 'pullreq_id', '')

        return fix_jobname('-'.join([prinfo, parts]))
    return fix_jobname(parts)
