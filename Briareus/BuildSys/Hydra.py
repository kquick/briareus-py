# Definitions for a Nix Hydra builder

import Briareus.BuildSys.BuilderBase as BuilderBase
from Briareus.Types import BuilderResult, BldConfig
from Briareus.BuildSys import buildcfg_name
import requests
import json
import os
from collections import Counter
from typing import List, Optional, Union


# For general admin processing of nix expressions
recent_nixpkgs = "https://github.com/NixOS/nixpkgs-channels nixos-19.09"


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

       Inputs:

         * conf_file :: builder_conf JSON file (or None)

         * builder_url :: URL of builder to fetch build results (default=None)

         * collate_inputs :: if True (the default), create a separate
                             project referencing the actual VCS inputs
                             (e.g. git) to aggregate any VCS accesses
                             into a single jobset instead of
                             duplicating across all jobsets for the
                             main project.  The main project will use
                             the build *outputs* from the inputs
                             project as inputs.

                             The advantage of this mode is faster
                             overall evaluation and less load on
                             updating local copies of repositories
                             from their remote version.  The
                             disadvantage is a more complicated
                             configuration and input changes require
                             extra indirection for identifying what
                             has changed.

    """

    builder_type = 'hydra'

    def __init__(self, *args, collate_inputs: bool = True, **kw):
        super(HydraBuilder, self).__init__(*args, **kw)
        self._collate_directive = collate_inputs

    def output_build_configurations(self, input_desc, bldcfgs,
                                    bldcfg_fname=None,
                                    verbose=False) -> BuilderBase.BuilderConfigsTy:
        """Given an input description and the set of build configurations
           generated from the BCGen logic, return the Hydra-specific
           configuration of those build configurations, along with any
           auxiliary files as a dictionary, where the key is the
           filename and the value is the contents; the key should be
           None for the primary output file, which is named in the
           input specification.

           For the Hydra builder, an auxiliary file is generated that
           can be used as the declarative project description (used
           for defining the Project in Hydra), along with a helper
           function to move that auxiliary file into the nix store on
           Hydra invocation.

           input_desc :: is the Briareus.Input.Description.InpDesc
                         object describing the repos, the branches,
                         and the variables.

           bldcfgs :: is the set of bldcfgs generated by the BCGen logic.

           bldcfg_fname :: is the main configuration file(+path) where
                           the output build configurations will be
                           written.  There may be additional files;
                           these will be intended to be written in the
                           same directory as the main configuration
                           file.  This routine does *not* actually
                           write to any files, but it generates output
                           that cross-references between the files at
                           this expected location.  If this argument
                           is None, then the project declarative
                           description and corresponding installation
                           nix file are not generated.

                           Note that if this parameter is *not*
                           supplied, then there is no location for
                           writing separate files, so only the primary
                           hydra configuration is returned and there
                           is also no collation of inputs.

        """
        input_cfg = (json.loads(open(self._conf_file, 'r').read())
                     if self._conf_file else {})
        project_name = input_cfg.get('project_name', input_desc.PNAME)
        gen_files_path = os.path.abspath(
            os.path.join(os.path.dirname(bldcfg_fname), 'hydra', project_name)) \
            if bldcfg_fname else None
        collated_inputs = self._collate_directive if bldcfg_fname else False
        vcs_inputs = VCSInputs(project_name, collated_inputs)

        for each in bldcfgs.cfg_build_configs:
            vcs_inputs.get_bldcfg_vcs_inputs(input_desc, bldcfgs, each)

        jobsets = { buildcfg_name(each) :
                           self._jobset(input_desc, bldcfgs, input_cfg,
                                        vcs_inputs, each)
                           for each in bldcfgs.cfg_build_configs }

        if bldcfg_fname is None:
            # Sort by key for output stability
            return {None: json.dumps({ n: jobsets[n].spec() for n in jobsets },
                                     sort_keys=True) }

        gen_files_pathsub = lambda s: os.path.abspath(
            os.path.join(os.path.dirname(bldcfg_fname), 'hydra', s))

        proj_cfgfname = os.path.join(gen_files_pathsub(project_name),
                                     project_name + '-hydra-project-config.json')
        mkpath = lambda fname: os.path.abspath(
            os.path.join(gen_files_pathsub(project_name), fname))

        if collated_inputs:
            projcfg_job = project_name + '-cfg'
            projcfgs = [ (projcfg_job, os.path.abspath(bldcfg_fname), 'path') ]

            if 'jobset' in input_cfg:
                for each in input_cfg['jobset'].get('inputs', dict()):
                    inpty = input_cfg['jobset']['inputs'][each]['type']
                    if inpty in [ 'git' ]:
                        projcfgs.append(
                            (each,
                             input_cfg['jobset']['inputs'][each]['value'],
                             inpty) )
                        for js in jobsets:
                            jobsets[js].add_input(
                                each,
                                ':'.join([project_name + '-inputs',
                                          vcs_inputs.jobset_name,
                                          each]),
                                'build')
                    else:
                        for js in jobsets:
                            jobsets[js].add_input(
                                each,
                                input_cfg['jobset']['inputs'][each]['value'],
                                inpty)

            update_inputs_jobset, vcs_files = \
                vcs_inputs.vcs_input_jobsets(gen_files_pathsub(project_name), projcfgs)
            inps_cfgfname = os.path.join(gen_files_pathsub(project_name),
                                         project_name + '-hydra-inputs-config.json')
            inpcfg_fname = os.path.join(gen_files_pathsub('_common'), "gen_inpupd.nix")
            inpcfg_body = '\n'.join([
                '{ nixpkgs, inpupd_jobset_def }:',
                '{ jobsets =',
                '    (import nixpkgs {}).stdenv.mkDerivation {',
                '      name = "gen_inpupd";',
                '      phases = [ "installPhase" ];',
                '      installPhase = "cp ${inpupd_jobset_def} $out";',
                '    };',
                '}', ''
            ])
            inpupd_jobset_fname = mkpath(project_name + "_inpupd_jobsets.json")
            inpupd_jobset_body = json.dumps(update_inputs_jobset)
            inp_cfgjobset = Jobset(
                'Briareus-generated %s Project inputs declaration'
                % project_name,
                os.path.basename(inpcfg_fname),
                "inpupd_cfg") \
                .add_input("inpupd_cfg", os.path.dirname(inpcfg_fname), 'path') \
                .add_input("inpupd_jobset_def", inpupd_jobset_fname,
                           'path') \
                .add_input('nixpkgs', recent_nixpkgs)

            projcfg_fname = os.path.join(gen_files_pathsub('_common'),
                                         "gen_proj_jobsets.nix")
            projcfg_body = '\n'.join([
                '{ nixpkgs, projcfg }:',
                '{ jobsets =',
                '    (import nixpkgs {}).stdenv.mkDerivation {',
                '      name = "gen_proj_jobsets";',
                '      phases = [ "installPhase" ];',
                '      installPhase = "cp ${projcfg} $out";',
                '    };',
                '}', ''
            ])
            proj_cfgjobset = Jobset(
                "Briareus-generated %s Project declaration" % project_name,
                os.path.basename(projcfg_fname),
                "mkjobsets") \
                .add_input("mkjobsets", os.path.dirname(projcfg_fname), 'path') \
                .add_input("projcfg",
                           ':'.join([project_name + "-inputs",
                                     list(update_inputs_jobset.keys())[0],
                                     projcfg_job]),
                           "build") \
                .add_input('nixpkgs', recent_nixpkgs)

            otherfiles = [
                ( proj_cfgfname, json.dumps(proj_cfgjobset.spec()) ),
                ( inps_cfgfname, json.dumps(inp_cfgjobset.spec()) ),
                ( inpcfg_fname, inpcfg_body ),
                ( inpupd_jobset_fname, inpupd_jobset_body ),
                ( projcfg_fname, projcfg_body ),
                ( project_name + ".txt",
                  '\n'.join([
                      'Instructions for configuring Hydra for the %s project:'
                      % project_name,
                      '',
                      '  1. Create a new project on the Hydra system',
                      '     a. Identifer = %s-inputs' % project_name,
                      '     b. Declarative spec file = %s'
                      % os.path.basename(inps_cfgfname),
                      '     c. Declarative input',
                      '         type: Local path or URL',
                      '         value: ' + os.path.abspath(
                          os.path.dirname(inps_cfgfname)),
                      '',
                      '  2. Create a new project on the Hydra system',
                      '     a. Identifer = %s' % project_name,
                      '     b. Declarative spec file = %s'
                      % os.path.basename(proj_cfgfname),
                      '     c. Declarative input',
                      '         type: Local path or URL',
                      '         value: ' + os.path.abspath(
                          os.path.dirname(proj_cfgfname)),
                      '',
                  ]) ),
            ] + vcs_files
        else:
            # Non-collated mode
            inpcfg_fname = os.path.join(gen_files_pathsub('_common'), "copy_hh.nix")
            projectdef_jobset = Jobset(
                "Briareus-generated %s Project declaration" % project_name,
                os.path.basename(inpcfg_fname),
                "copy_hh_src") \
                .add_input('hh_output', mkpath(bldcfg_fname), 'path') \
                .add_input('copy_hh_src', os.path.dirname(inpcfg_fname), 'path') \
                .add_input('nixpkgs', recent_nixpkgs)

            if 'jobset' in input_cfg:
                for each in input_cfg['jobset'].get('inputs', dict()):
                    for js in jobsets:
                        jobsets[js].add_input(
                            each,
                            input_cfg['jobset']['inputs'][each]['value'],
                            input_cfg['jobset']['inputs'][each]['type'])

            vcs_jobsets, vcs_files = vcs_inputs.vcs_input_jobsets(gen_files_path)
            jobsets.update(vcs_jobsets)

            otherfiles = [
                ( proj_cfgfname, json.dumps(projectdef_jobset.spec()) ),
                ( mkpath(inpcfg_fname),
                  '\n'.join([
                      '{ nixpkgs, hh_output }:',
                      '{ jobsets = (import <nixpkgs> {}).stdenv.mkDerivation {',
                      '    name = "copy_hh";',
                      '    phases = [ "installPhase" ];',
                      '    installPhase = "cp ${hh_output} $out";',
                      '  };',
                      '}', '',
                  ]) ),
                ( project_name + ".txt",
                  '\n'.join([
                      'Instructions for configuring Hydra for the %s project:'
                      % project_name,
                      '',
                      '  1. Create a new project on the Hydra system',
                      '     a. Identifer = %s' % project_name,
                      '     b. Declarative spec file = %s'
                      % os.path.basename(proj_cfgfname),
                      '     c. Declarative input',
                      '         type: Local path or URL',
                      '         value: ' + os.path.abspath(
                          os.path.dirname(proj_cfgfname)),
                      '',
                  ]) ),
            ] + vcs_files

        if verbose:
            print('## Generating', len(jobsets), 'Hydra jobsets,',
                  'input compression', vcs_inputs.verbose_info())

        return dict([ (None, json.dumps({ n: jobsets[n].spec()
                                          for n in jobsets },
                                        # Sort by key for output stability
                                        sort_keys=True)) ] + otherfiles)

    def _jobset(self, input_desc, bldcfgs, input_cfg, vcs_inputs, bldcfg):
        projrepo = [ r for r in input_desc.RL if r.project_repo ][0]
        ret = Jobset(self._jobset_desc(bldcfgs, bldcfg),
                     "./release.nix",  # the hydra convention
                     projrepo.repo_name + "-src",  # must be an input
        )
        ret = self._jobset_add_inputs(ret, input_desc, bldcfgs, vcs_inputs,
                                      bldcfg)
        ret.update_fields(input_cfg.get('jobset', {}))
        return ret

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

    def _jobset_desc(self, bldcfgs, bldcfg):
        brr_info = []
        for brr in sorted(bldcfg.blds):  # BuildConfigs.BuildRepoRev
            preq = pullreq_for_bldcfg_and_brr(bldcfgs, bldcfg, brr)
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

    def _jobset_add_inputs(self, jobset, input_desc, bldcfgs, vcs_inputs,
                           bldcfg):
        for v in bldcfg.bldvars:
            jobset.add_input(v.varname, v.varvalue, 'string')
        vcs_inputs.add_vcs_inputs(jobset, bldcfgs, bldcfg, bldcfg.blds)
        jobset.add_input('variant', self._jobset_variant(bldcfg), 'string')
        return jobset

    def update(self, cfg_spec):
        print("Takes output of output_build_configurations"
              " and updates the actual remote builder")

    def get_project_url(self, project: str) -> Optional[BuilderBase.BuilderURL]:
        if not self._builder_url:
            return None
        return BuilderBase.BuilderURL('/'.join([self._builder_url, 'project', project]))

    def _get_build_results(self):
        r = getattr(self, '_build_results', None)
        if not r:
            if not self._builder_url:
                return ('Build results cannot be retrieved'
                        ' without a builder URL')
            if not self._conf_file:
                return ('Build results cannot be retrieved'
                        ' without builder configuration information.')
            input_cfg = json.loads(open(self._conf_file, 'r').read())
            project_name = input_cfg.get('project_name', None)
            if not project_name:
                return 'Build results require a project_name for querying Hydra'
            url = self._builder_url + "/api/jobsets?project=" + project_name
            r = requests.get(url)
            if r.status_code == 404:
                return 'No build results at specified target (%s)' % url
            r.raise_for_status()
            self._build_results = project_name, r.json()
        return self._build_results


    def get_build_result(self, bldcfg: BldConfig) -> Union[str, BuilderResult]:
        n = buildcfg_name(bldcfg)
        rval = self._get_build_results()
        if isinstance(rval, str):
            return rval
        project_name, r = rval
        bldres = [
            BuilderResult(
                buildname=n,
                nrtotal=get_or_show(e, 'nrtotal'),
                nrsucceeded=get_or_show(e, 'nrsucceeded'),
                nrfailed=get_or_show(e, 'nrfailed'),
                nrscheduled=get_or_show(e, 'nrscheduled'),
                cfgerror=(get_or_show(e, 'haserrormsg') or
                          bool(get_or_show(e, "fetcherrormsg"))),
                builder_url = (BuilderBase.BuilderURL("/".join([self._builder_url,
                                                                "jobset",
                                                                project_name, n]))
                               if self._builder_url else None),
            )
            for e in r if e['name'] == n
        ]
        return bldres[0] if bldres else ('No results available for jobset ' + n)


def get_or_show(obj, fieldname):
    if fieldname not in obj:
        print('Missing field "%s" in builder result: %s'
              % ( fieldname, str(obj) ))
    return obj.get(fieldname)


def VCS_repo_url(input_desc, bldcfgs, bldreporev):
    for each in input_desc.RL:
        if each.repo_name == bldreporev.reponame:
            return each.repo_url
    for each in bldcfgs.cfg_subrepos:  # RepoDesc
        if each.repo_name == bldreporev.reponame:
            return each.repo_url
    return '--<unknown URL for repo %s>--' % bldreporev.reponame


def pullreq_for_bldcfg_and_brr(bldcfgs, bldcfg, brr):
    return (([ p for p in bldcfgs.cfg_pullreqs  # VCS_API.PRInfo
               if p.pr_target_repo == brr.reponame and
                  p.pr_branch == bldcfg.branchname and
                  p.pr_ident == brr.pullreq_id
             ] + [None])[0]
            if bldcfg.branchtype == 'pullreq' and
               brr.pullreq_id != 'project_primary'
            else None)


class VCSInputs(object):
    """Collects all of the VCS inputs so that shared VCS inputs can be
       aggregated so that hydra-evaluator only needs to check the
       remote VCS once for this project instead of once for each
       jobset in the project using the input.  The inputs are then
       output as separate, hidden jobsets.

    """
    def __init__(self, project_name, collated_input_jobset=True):
        self._project_name = project_name
        self._collated = collated_input_jobset
        self.jobset_name = 'update_inputs'
        self._inputs = {}
        self._inputcount = Counter()
        self.add_vcs_inputs = self._add_collated_vcs_inputs \
            if self._collated else self._add_distinct_vcs_inputs
        self.vcs_input_jobsets = self._collated_vcs_input_jobsets \
            if self._collated else self._distinct_vcs_input_jobsets

    @staticmethod
    def key_for(bldcfgs, bldcfg, brr):
        mbpr = pullreq_for_bldcfg_and_brr(bldcfgs, bldcfg, brr)
        # The pr is relative to a specific repo and this is a
        # determination of the key name for the source VCS reference
        # in that repo, so the pr_ident will be unique in this
        # context and is therefore safe to use to uniquely specify this key.
        return (brr.reponame, 'PR%s' % mbpr.pr_ident if mbpr else brr.repover)

    def get_bldcfg_vcs_inputs(self, input_desc, bldcfgs, bldcfg):
        repo_url_maybe_pullreq = lambda brr, mbpr: \
            (mbpr.pr_srcrepo_url
             if mbpr else
             VCS_repo_url(input_desc, bldcfgs, brr))
        repo_url = lambda brr: \
            repo_url_maybe_pullreq(brr,
                                   pullreq_for_bldcfg_and_brr(bldcfgs,
                                                              bldcfg,
                                                              brr))
        for each in bldcfg.blds:
            key = self.key_for(bldcfgs, bldcfg, each)
            self._inputs[key] = (repo_url(each), each.repover)
            self._inputcount[key] += 1

    def _add_distinct_vcs_inputs(self, jobset, bldcfgs, bldcfg, blds):
        for each in blds:
            jobset.add_input(
                each.reponame + '-src',
                ' '.join(list(self._inputs[self.key_for(bldcfgs,
                                                        bldcfg,
                                                        each)])))
        return jobset

    def _add_collated_vcs_inputs(self, jobset, bldcfgs, bldcfg, blds):
        for each in blds:
            jobset.add_input(
                each.reponame + "-src",
                ':'.join(
                    [self._project_name + '-inputs',
                     self.jobset_name,
                     self._collated_input_name(self.key_for(bldcfgs,
                                                            bldcfg,
                                                            each))]),
                'build')
        return jobset

    def vcs_input_jobsets(self, gen_files_path,
                          additional_passthru_inputs=[]):
        if self._collated:
            return self._collated_vcs_input_jobsets(gen_files_path,
                                                    additional_passthru_inputs)
        return self._distinct_vcs_input_jobsets(gen_files_path, additional_passthru_inputs)

    def _distinct_vcs_input_jobsets(self, gen_files_path,
                                    additional_passthru_inputs=[]):
        "Old style where inputs are in the main jobsets"
        assert additional_passthru_inputs == []
        return {}, []

    @staticmethod
    def _collated_input_name(key_for):
        return '-'.join(list(key_for)).replace('.','----').replace('/','--').replace('_','---')

    def _collated_vcs_input_jobsets(self, gen_files_path,
                                    additional_passthru_inputs=[]):
        fname = self._project_name + '-' + "updinputs" + '.nix'
        jobset = Jobset('Check for actual input updates', fname,
                        'realize_inputs') \
            .add_input('realize_inputs', gen_files_path, 'path') \
            .add_input('nixpkgs', recent_nixpkgs)
        for eachkey in self._inputs:
            jobset.add_input(self._collated_input_name(eachkey) + "-src",
                             ' '.join(list(self._inputs[eachkey])))
        for nm,vl,ty in additional_passthru_inputs:
            jobset.add_input(nm + "-inp", vl, ty)

        nixfiles = [
            (os.path.join(gen_files_path, fname),
             '\n'.join([
                 '{ nixpkgs,'
             ] + [
                 self._collated_input_name(eachkey) + "-src,"
                 for eachkey in self._inputs
             ] + [
                 nm + "-inp," for nm,_,_ in additional_passthru_inputs
             ] + [
                 'last ? null',
                 '}:',
                 'let gen = name: inp:',
                 '  (import nixpkgs {}).stdenv.mkDerivation {',
                 '    name = name;',
                 '    phases = [ "installPhase" ];',
                 '    installPhase = "cp -r ${inp} $out";',
                 '  };',
                 'in',
                 '{',
             ] + [
                 '%s = gen "%s" %s;'
                 % (self._collated_input_name(eachkey),
                    self._collated_input_name(eachkey),
                    self._collated_input_name(eachkey) + "-src")
                 for eachkey in self._inputs
             ] + [
                 '%s = gen "%s" %s;' % (nm, nm, nm + "-inp")
                 for nm,_,_ in additional_passthru_inputs
             ] + [
                 '}',
                 '',
             ])),
        ]

        return { self.jobset_name: jobset.spec() }, nixfiles

    def verbose_info(self):
        return str(dict(self._inputcount))


class Jobset(object):
    def __init__(self, description, nixexprpath, nixexprinput,
                 checkinterval=600):
        self._fields = { 'description': description,
                         'nixexprpath': nixexprpath,
                         'nixexprinput': nixexprinput,
                         'checkinterval': checkinterval,
                         'keepnr': 3,
                         'schedulingshares': 1,
                         'emailoverride': '',
                         'enabled': 1,
                         'hidden': False,
                         'enableemail': False,
                         }
        self._inputs = []

    def update_fields(self, newfields):
        self._fields.update(newfields)

    def add_input(self, name, value, inptype='git'):
        self._inputs.append( (name,
                              { 'type': inptype,
                                'value': value,
                                'emailresponsible': False,
                              }) )
        return self

    def spec(self):
        return dict(list(self._fields.items()) + [('inputs', dict(self._inputs))])
