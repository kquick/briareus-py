# Definitions for a Nix Hydra builder

import Briareus.BuildSys.BuilderBase as BuilderBase
from Briareus.Types import BuilderResult
import requests
import json


class HydraBuilder(BuilderBase.Builder):
    """Generates Hydra jobset output for each build configuration.  The
       jobset specifies each BldRepoRev as a separate input to the
       jobset (with the suffix "-src").

       The builder_conf file should be a JSON file specifying any
       overrides for jobset fields (and the inputs section will be
       supplemental to (and override similar inputs) in the
       build-config-generated inputs.

          { "jobset" : {
                ... jobset overrides ...,
                "inputs": {
                    ... additional inputs/overrides ...
                }
            }
          }
    """

    builder_type = 'hydra'

    def output_build_configurations(self, input_desc, bldcfgs):
        input_cfg = (json.loads(open(self._conf_file, 'r').read())
                     if self._conf_file else {})
        return json.dumps(
            { self._jobset_name(each) :
              self._jobset(input_desc, bldcfgs, input_cfg, each)
              for each in bldcfgs.cfg_build_configs })

    def _jobset(self, input_desc, bldcfgs, input_cfg, bldcfg):
        jobset_inputs = self._jobset_inputs(input_desc, bldcfgs, bldcfg)
        if 'jobset' in input_cfg and 'inputs' in input_cfg['jobset']:
            jobset_inputs.update(input_cfg['jobset']['inputs'])
        projrepo = [ r for r in input_desc.RL if r.project_repo ][0]
        jobset = {
            # These are the defaults which can be overridden by
            # input_cfg which was the passed in builder_config file.
            "checkinterval": 600,  # 5 minutes
            "description": self._jobset_desc(bldcfgs, bldcfg),
            "emailoverride": "",
            "enabled": 1,
            "enableemail": False,
            "hidden": False,
            "keepnr": 3,  # number of builds to keep
            "nixexprinput": projrepo.repo_name + "-src",  # must be an input
            "nixexprpath": "./release.nix",  # the hydra convention
            "schedulingshares": 1,
            }
        jobset.update(input_cfg.get('jobset', {}))
        jobset['inputs'] = jobset_inputs
        return jobset


    def _jobset_name(self, bldcfg):
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
            # n.b. a PR jobset is *not* identified by the PR number
            # (pr_ident) because the PR number is repo-specific it's
            # possible that the PR exists for multiple repos.  Any
            # PR's that share the same branch name are assumed to be
            # related and built together, so there may be many PR's.
            #
            # The assumed workflow is that a PR in one repo will cause
            # build failures in a downstream repo, which will be
            # addressed by creating an identically-named branch in the
            # downstream repo for the fixes and eventually turning
            # that into a PR in that repo as well.
            #
            # If the jobset name were to change as new PRs were
            # created, it would make it difficult to track overall
            # progress relative to the PR, so the numbers are
            # available in the description, but not in the jobset name
            # which is used for correlation purposes (in both Briareus
            # and Hydra).
            return '-'.join(["PR",
                             '.'.join([fix_branchname(bldcfg.branchname),
                                       bldcfg.strategy])] +
                            varparts)
        return '-'.join(['.'.join([fix_branchname(bldcfg.branchname),
                                   bldcfg.strategy])] +
                        varparts)

    def _jobset_variant(self, bldcfg):
        # Provides a string "variant" input to the jobset to allow the
        # jobset to customize itself (e.g. selecting different
        # dependencies for a branch v.s. the master).  Provide various
        # jobset information in a regular fashion to allow easy
        # interpretation by the nix jobset expression:
        #
        #   "|key=value|key=value..."
        #
        # Variables do not need to be part of the variant because they
        # are already independently specified.
        #
        # Similarly, BldRepoRev translates into independent inputs.
        return '|' + '|'.join(
            filter(None,
                   [ 'branch=' + bldcfg.branchname,
                     'strategy=' + bldcfg.strategy,
                     'PR' if bldcfg.branchtype == "pullreq" else None,
                   ]))

    def _pullreq_for_bldcfg_and_brr(self, bldcfgs, bldcfg, brr):
        return (([ p for p in bldcfgs.cfg_pullreqs  # InternalOps.PRInfo
                   if p.pr_target_repo == brr.reponame and p.pr_branch == bldcfg.branchname] + [None])[0]
                if bldcfg.branchtype == 'pullreq' else None)

    def _jobset_desc(self, bldcfgs, bldcfg):
        brr_info = []
        for brr in sorted(bldcfg.blds):  # BuildConfigs.BuildRepoRev
            preq = self._pullreq_for_bldcfg_and_brr(bldcfgs, bldcfg, brr)
            if preq:
                brr_info.append( "PR%(pr_ident)s-brr%%(srcident)s:%%(reponame)s"
                                 % preq.__dict__ % brr.__dict__ )
                continue
            brr_info.append( "brr%(srcident)s:%(reponame)s" % brr.__dict__ )

        return ("Build configuration: " +
                ", ".join(brr_info +
                          [ "%(varname)s=%(varvalue)s" % v.__dict__
                            for v in sorted(bldcfg.bldvars) ]
                ))

    def _jobset_inputs(self, input_desc, bldcfgs, bldcfg):
        repo_url_maybe_pullreq = lambda brr, mbpr: \
            (mbpr.pr_srcrepo_url
             if mbpr else
             self._repo_url(input_desc, bldcfgs, brr))
        repo_url = lambda brr: \
            repo_url_maybe_pullreq(brr,
                                   self._pullreq_for_bldcfg_and_brr(bldcfgs,
                                                                    bldcfg,
                                                                    brr))
        return dict(
            [ ('variant',
               {
                   'emailresponsible': False,
                   'type': 'string',
                   'value': self._jobset_variant(bldcfg)
               }),
            ] +
            [ (each.reponame + "-src",
               {
                   "emailresponsible": False,
                   "type": "git",
                   "value": ' '.join([repo_url(each), each.repover,])
               }) for each in bldcfg.blds ] +
            [ (v.varname, {
                "emailresponsible": False,
                "type": "string",
                "value": v.varvalue
            }) for v in bldcfg.bldvars ]
        )

    def _repo_url(self, input_desc, bldcfgs, bldreporev):
        for each in input_desc.RL:
            if each.repo_name == bldreporev.reponame:
                return each.repo_url
        for each in bldcfgs.cfg_subrepos:  # RepoDesc
            if each.repo_name == bldreporev.reponame:
                return each.repo_url
        return '--<unknown URL for repo %s>--' % bldreporev.reponame

    def update(self, cfg_spec):
        print("Takes output of output_build_configurations and updates the actual remote builder")

    def _get_build_results(self):
        r = getattr(self, '_build_results', None)
        if not r:
            if not self._builder_url:
                return 'Build results cannot be retrieved without a builder URL'
            if not self._conf_file:
                return 'Build results cannot be retrieved without builder configuration information.'
            input_cfg = json.loads(open(self._conf_file, 'r').read())
            project_name = input_cfg.get('project_name', None)
            if not project_name:
                return 'Build results require a project_name for querying Hydra'
            url = self._builder_url + "/api/jobsets?project=" + project_name
            r = requests.get(url)
            if r.status_code == 404:
                return 'No build results at specified target (%s)' % url
            r.raise_for_status()
            self._build_results = r.json()
        return self._build_results


    def get_build_result(self, bldcfg):
        n = self._jobset_name(bldcfg)
        r = self._get_build_results()
        if isinstance(r, str):
            return r
        return ([
            BuilderResult(
                buildname=n,
                nrtotal=get_or_show(e, 'nrtotal'),
                nrsucceeded=get_or_show(e, 'nrsucceeded'),
                nrfailed=get_or_show(e, 'nrfailed'),
                nrscheduled=get_or_show(e, 'nrscheduled'),
                cfgerror=get_or_show(e, 'haserrormsg'),
            )
            for e in r if e['name'] == n
        ] + ['No results available for jobset ' + n])[0]

def get_or_show(obj, fieldname):
    if fieldname not in obj:
        print('Missing field "%s" in builder result: %s'
              % ( fieldname, str(obj) ))
    return obj.get(fieldname)
