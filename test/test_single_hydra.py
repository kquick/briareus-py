import json
import pytest
from helpers import *
import Briareus.BuildSys.Hydra as HydraBldSys
from hydra_verification import (VerifyJobsetDesc, VerifyBldCfg, VerifyInput,
                                VerifyProjectCfg)
from test_single import input_spec, expected_repo_info


def test_single_hydrasetup_nopath(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None)
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  # bldcfg_fname='/input/bldcfg/path/fname',
                                                  verbose=True)
    assert_eqlist(bldcfgs.keys(), [None])
    verify_primary_cfg_uncollated('unused', bldcfgs)

def test_single_hydrasetup_nopath_uncollated(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None, collate_inputs=False)
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  # bldcfg_fname='/input/bldcfg/path/fname',
                                                  verbose=True)
    assert_eqlist(bldcfgs.keys(), [None])
    verify_primary_cfg_uncollated('unused', bldcfgs)

def test_single_hydrasetup_nopath_collated(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None, collate_inputs=True)
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  # bldcfg_fname='/input/bldcfg/path/fname',
                                                  verbose=True)
    assert_eqlist(bldcfgs.keys(), [None])
    verify_primary_cfg_uncollated('unused', bldcfgs)



def verify_primary_cfg_uncollated(bpath, bldcfgs):

    assert None in bldcfgs
    other_bldcfgs = [x for x in bldcfgs.keys() if x is not None]
    # Either none or a specific set
    if other_bldcfgs:
        assert_eqlist(other_bldcfgs,
                      [bpath + '/hydra/copy_hh.nix',
                       'TheRepo-hydra-project-config.json',
                       'TheRepo.txt',
                      ])

    VerifyJobsetDesc(
        VerifyInput('variant', 'string', 'strategy', contains=True),
        VerifyBldCfg('PR-frog.standard',
                     VerifyInput('TheRepo-src', 'git', 'frog_repo_url frog'),
                     VerifyInput('variant', 'string', 'PR', contains=True)),
        VerifyBldCfg('PR-toad.standard',
                     VerifyInput('TheRepo-src', 'git', 'toad_repo_url toad'),
                     VerifyInput('variant', 'string', 'PR', contains=True)),
        VerifyBldCfg('feat1.standard',
                     VerifyInput('TheRepo-src', 'git', 'the_repo_url feat1')),
        VerifyBldCfg('master.standard',
                     VerifyInput('TheRepo-src', 'git', 'the_repo_url master')),
        nixexprinput = 'TheRepo-src',
        nixexprpath = './release.nix',
    ).verify(bldcfgs[None])

    if other_bldcfgs:
        verify_cfg_instructions(bldcfgs['TheRepo.txt'])
        # assert bldcfgs['TheRepo.txt'] == 'moo'

        # Specifies the project .jobsets configuration that generates
        # the JSON file describing the jobsets
        VerifyProjectCfg(
            VerifyInput('nixpkgs', 'git',
                        'https://github.com/NixOS/nixpkgs-channels nixos-19.09'),
            # The copy_hh_src input is needed to copy the output
            # briareus config file (bldcfgs[None]) to the nix store
            VerifyInput('copy_hh_src', 'path', bpath + '/hydra/_common'),
            # The hh_output input is the source for the copy operation
            VerifyInput('hh_output', 'path', bpath + '/fname'),
            nixexprinput = 'copy_hh_src',
            nixexprpath = 'copy_hh.nix',
        ).verify(json.loads(bldcfgs['TheRepo-hydra-project-config.json']))

        assert_in('cp ${hh_output} $out',
                  bldcfgs[bpath + '/hydra/copy_hh.nix']) : used?!


def verify_cfg_instructions(instrs):
    # Should be some instructions... don't bother to check their details
    assert instrs


def test_single_hydrasetup_bldcfg(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None)
    bpath = '/input/bldcfg/path'
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  bldcfg_fname=bpath + '/fname',
                                                  verbose=True)
    assert_in(None, bldcfgs.keys())
    verify_primary_cfg_collated(bpath, bldcfgs)

def test_single_hydrasetup_bldcfg_collated(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None, collate_inputs=True)
    bpath = '/input/bldcfg/path'
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  bldcfg_fname=bpath + '/fname',
                                                  verbose=True)
    assert_in(None, bldcfgs.keys())
    verify_primary_cfg_collated(bpath, bldcfgs)

def test_single_hydrasetup_bldcfg_uncollated(generated_repo_info, generated_bldconfigs):
    input_desc, repo_info = generated_repo_info
    builder = HydraBldSys.HydraBuilder(None, collate_inputs=False)
    bpath = '/input/bldcfg/path'
    bldcfgs = builder.output_build_configurations(input_desc,
                                                  generated_bldconfigs,
                                                  bldcfg_fname=bpath + '/fname',
                                                  verbose=True)
    assert_in(None, bldcfgs.keys())
    verify_primary_cfg_uncollated(bpath, bldcfgs)


def verify_primary_cfg_collated(bpath, bldcfgs):

    admin_nixpkgs = VerifyInput('nixpkgs', 'git',
                                'https://github.com/NixOS/nixpkgs-channels nixos-19.09')

    VerifyJobsetDesc(
        VerifyInput('variant', 'string', 'strategy', contains=True),
        VerifyBldCfg('PR-frog.standard',
                     VerifyInput('TheRepo-src', 'build',
                                 'TheRepo-inputs:update_inputs:TheRepo-PR91'),
                     VerifyInput('variant', 'string', 'PR', contains=True)),
        VerifyBldCfg('PR-toad.standard',
                     VerifyInput('TheRepo-src', 'build',
                                 'TheRepo-inputs:update_inputs:TheRepo-PR134'),
                     VerifyInput('variant', 'string', 'PR', contains=True)),
        VerifyBldCfg('feat1.standard',
                     VerifyInput('TheRepo-src', 'build',
                                 'TheRepo-inputs:update_inputs:TheRepo-feat1')),
        VerifyBldCfg('master.standard',
                     VerifyInput('TheRepo-src', 'build',
                                 'TheRepo-inputs:update_inputs:TheRepo-master')),
        nixexprinput = 'TheRepo-src',
        nixexprpath = './release.nix',
    ).verify(bldcfgs[None])

    assert_eqlist([K for K in bldcfgs.keys() if K is not None],
                  [ 'TheRepo-hydra-project-config.json',
                    'TheRepo-hydra-inputs-config.json',
                    bpath + '/hydra/gen_inpupd.nix',
                    bpath + '/hydra/TheRepo_inpupd_jobsets.json',
                    bpath + '/hydra/TheRepo-updinputs.nix',
                    bpath + '/hydra/gen_proj_jobsets.nix',
                    'TheRepo.txt',
                  ])

    verify_cfg_instructions(bldcfgs['TheRepo.txt'])

    # The project config file is used as the declarative input for the
    # project itself.  The jobsets should be generated by the
    # gen_proj_jobsets.nix file generated in this same output set.
    VerifyProjectCfg(
        admin_nixpkgs,
        # The input which contains the primary expression
        VerifyInput('mkjobsets', 'path', bpath + '/hydra'),
        # This is the actual .jobsets, which is copied to the store by the nixexprpath
        VerifyInput('projcfg', 'build', 'TheRepo-inputs:update_inputs:TheRepo-cfg'),
        nixexprinput='mkjobsets',
        nixexprpath='gen_proj_jobsets.nix',
    ).verify(json.loads(bldcfgs['TheRepo-hydra-project-config.json']))

    # The nix expression to generate the .jobsets for the project.  It
    # should copy the projcfg input to the store
    gen_proj_jobsets = bldcfgs[bpath + '/hydra/gen_proj_jobsets.nix']
    assert_in('cp ${projcfg} $out', gen_proj_jobsets)

    # The inputs config file is used as the declarative input for the
    # inputs project.  The jobsets should be generated by the
    # gen_inpupd.nix file generated in this same output set.
    VerifyProjectCfg(
        # The input which contains the primary expression
        VerifyInput('inpupd_cfg', 'path', bpath + '/hydra'),
        # This is the actual .jobsets, which is copied to the store by
        # the nixexprpath
        VerifyInput('inpupd_jobset_def', 'path',
                    bpath + '/hydra/TheRepo_inpupd_jobsets.json'),
        # nixpkgs is needed for the gen_proj_jobsets.nix expression
        admin_nixpkgs,
        nixexprinput = 'inpupd_cfg',
        nixexprpath = 'gen_inpupd.nix',
    ).verify(json.loads(bldcfgs['TheRepo-hydra-inputs-config.json']))

    # The gen_inpupd.nix file provides the expression for defining the
    # .jobset output for the inputs project.
    gen_inpupd = bldcfgs[bpath + '/hydra/gen_inpupd.nix']
    assert_in('cp ${inpupd_jobset_def} $out', gen_inpupd)

    # The nix expression to generate the .jobsets for the input project.  It
    # should copy the projcfg input to the store
    VerifyJobsetDesc(
        VerifyBldCfg("update_inputs",
                     # realize_inputs is where the nixexprpath is obtained from
                     VerifyInput('realize_inputs', 'path', bpath + '/hydra'),
                     # TheRepo-cfg-inp is KWQ? to re-run this if the input config changes??
                     VerifyInput('TheRepo-cfg-inp', 'path', bpath + '/fname'),
                     VerifyInput('TheRepo-PR91-src', 'git', 'frog_repo_url frog'),
                     VerifyInput('TheRepo-PR134-src', 'git', 'toad_repo_url toad'),
                     VerifyInput('TheRepo-feat1-src', 'git', 'the_repo_url feat1'),
                     VerifyInput('TheRepo-master-src', 'git', 'the_repo_url master'),
                     # nixpkgs is needed for the KWQ? expression
                     admin_nixpkgs,
        ),
        nixexprinput = 'realize_inputs',
        nixexprpath = 'TheRepo-updinputs.nix',
    ).verify(bldcfgs[bpath + '/hydra/TheRepo_inpupd_jobsets.json'])

    # The TheRepo_updinputs.nix is the main nix expression that is
    # associated with the update_inputs jobset.
    updinp_expr = bldcfgs[bpath + '/hydra/TheRepo-updinputs.nix']
    assert_in('TheRepo-PR91-src', updinp_expr)
    assert_in('TheRepo-PR134-src', updinp_expr)
    assert_in('TheRepo-feat1-src', updinp_expr)
    assert_in('TheRepo-master-src', updinp_expr)
   : this performs a cp to the store for each; is this an
    # unnecessary duplication since the input should already be in the
    # store, or is this necessary to make the output of this jobset be
    # available?
