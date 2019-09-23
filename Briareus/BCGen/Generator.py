# Core BCGen functionality to process input specification into build configurations

from Briareus.BCGen.BuildConfigs import logic_result_expr
from Briareus.BCGen.Logic import DeclareFact, Fact, run_logic_analysis
import attr


@attr.s(frozen=True)
class GeneratedConfigs(object):
    cfg_build_configs = attr.ib()
    cfg_subrepos = attr.ib(factory=set)
    cfg_pullreqs = attr.ib(factory=set)


class Generator(object):
    def __init__(self, actor_system=None, verbose=False):
        self._actor_system = actor_system
        self.verbose = verbose

    def generate_build_configs(self, input_descr, repo_info, up_to=None):
        """The core process of generating build_config information from an
           input description.  The up_to argument can request early
           return with the information "up-to" a specific point; this
           is primarily used for diagnostics and testing.
        """
        facts, subrepos, pullreqs = self.get_input_facts(input_descr.RL,
                                                         input_descr.BL,
                                                         input_descr.VAR,
                                                         repo_info)
        if self.verbose or up_to == 'facts':
            print('## FACTS:')
            for f in facts:
                print(str(f))
        if up_to == "facts":
            return (up_to, facts)
        r = run_logic_analysis('build_config', facts,
                               actor_system=self._actor_system,
                               verbose=self.verbose)
        if self.verbose or up_to == 'raw_logic_output':
            print('## RAW_LOGIC_OUTPUT:')
            print(str(r))
        if up_to == "raw_logic_output":
            return (up_to, r)
        if not r:
            return ([], [])
        return ("build_configs",
                GeneratedConfigs(eval(r, globals(), logic_result_expr),
                                 subrepos,
                                 pullreqs))


    def get_input_facts(self, RL, BL, VAR, info):

        if not RL:
            return BuildConfigs.BldConfigSet()  # dummy run, build nothing

        projects = [ r for r in RL if r.project_repo ]

        if len(projects) > 1:
            raise AssertionError("only one 'project' allowed in input specification.")
        project = projects[0] if projects else None

        declare_facts = [ DeclareFact('repo/1'),
                          DeclareFact('project/1'),
                          DeclareFact('branch/1'),
                          DeclareFact('subrepo/1'),
                          DeclareFact('pullreq/3'),
                          DeclareFact('branch/2'),
                          DeclareFact('submodule/4'),
                          DeclareFact('varname/1'),
                          DeclareFact('var/2'),
        ]

        repo_facts    = [ Fact('repo("%s")'    % r.repo_name)   for r in RL ]
        project_facts = [ Fact('project("%s")' % r.repo_name)   for r in projects ]
        branch_facts  = [ Fact('branch("%s")'  % b.branch_name) for b in BL ]
        subrepo_facts = [ Fact('subrepo("%s")' % r.repo_name)   for r in info['subrepos'] ]

        # n.b. info['pullreqs'] are of type PRInfo from InternalOps;
        # the actual definition is not imported here because Python is
        # duck-typed.
        pullreqs = info['pullreqs']
        pullreq_facts = [ Fact('pullreq("%(pr_target_repo)s", "%(pr_ident)s", "%(pr_branch)s")' %
                               p.__dict__)
                          for p in pullreqs ]

        # n.b. See note in InternalOps: a pullreq for a repo
        # "overrides" a similarly-named branch for that repo, so in
        # general a pullreq will suppress a branch declaration, but it
        # will cause a check on all other repositories (including
        # other projects sharing this repository) for the branch.

        repobranch_facts = [ Fact('branch("%s", "%s")' % rb) for rb in info['branches'] ]

        submodules_facts = []
        if project:
            pn = project.repo_name
            # n.b. info['subrepos'] are of type SubRepoInfo from InternalOps;
            # the actual definition is not imported here because Python is
            # duck-typed.
            submods_data = lambda bname: [ (e.sm_sub_name, e.sm_sub_vers) for e in info['submodules']
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
                varval_facts,
                info['subrepos'], info['pullreqs'])
