import os
import json
import pytest
from helpers import *
import Briareus.BuildSys.Hydra as HydraBldSys
import Briareus.hh as hh
from hydra_verification import (VerifyJobsetDesc, VerifyBldCfg, VerifyInput,
                                VerifyProjectCfg)
from test_example2_add import testing_dir, inp_configs


def test_ex2add_hydra_projcnt(generated_inp_config_bldconfigs):
    assert len(generated_inp_config_bldconfigs.result_sets) == 2


def test_ex2add_hydra_proj1(generated_inp_config_bldconfigs):
    builder = HydraBldSys.HydraBuilder(None)
    assert len(generated_inp_config_bldconfigs.result_sets) == 2
    projset = generated_inp_config_bldconfigs.result_sets[0]
    bldcfgs = builder.output_build_configurations(
        projset.inp_desc,
        projset.build_cfgs,
        bldcfg_fname='test/inp_example2',
        verbose=True)
    # bldcfgs = projset.build_cfgs
    assert_eq(projset.inp_desc.PNAME, 'Repo1')
    verify_repo1_hydracfg(os.path.join(os.getcwd(), 'test'), bldcfgs)


def test_ex2add_hydra_withpath(inp_configs, generated_inp_config_bldconfigs):
    builder = HydraBldSys.HydraBuilder(None)
    projset = generated_inp_config_bldconfigs.result_sets[1]
    bldcfgs = builder.output_build_configurations(
        projset.inp_desc,
        projset.build_cfgs,
        bldcfg_fname='test/inp_example2_add',
        verbose=True)
    # bldcfgs = projset.build_cfgs
    assert_eq(projset.inp_desc.PNAME, 'alt-proj')
    verify_alt_proj_hydracfg(os.path.join(os.getcwd(), 'test'), bldcfgs)


admin_nixpkgs = VerifyInput('nixpkgs', 'git',
                            'https://github.com/NixOS/nixpkgs-channels nixos-19.09')


# ----------------------------------------------------------------------

def verify_repo1_hydracfg(bpath, bldcfgs):

    assert None in bldcfgs
    other_bldcfgs = [x for x in bldcfgs.keys() if x is not None]
    assert_eqlist(other_bldcfgs,
                  [ 'Repo1-hydra-project-config.json',
                    'Repo1-hydra-inputs-config.json',
                    os.path.join(bpath, 'hydra/gen_inpupd.nix'),
                    os.path.join(bpath, 'hydra/Repo1_inpupd_jobsets.json'),
                    os.path.join(bpath, 'hydra/Repo1-updinputs.nix'),
                    os.path.join(bpath, 'hydra/gen_proj_jobsets.nix'),
                    'Repo1.txt',
                  ])

    VerifyJobsetDesc(
        VerifyInput('variant', 'string', 'strategy', contains=True),
        *tuple([
            VerifyBldCfg('develop.HEADs-' + ghcver,
                     # VerifyInput('Repo1-src', 'git', 'r1_url develop'),
                     # VerifyInput('Repo2-src', 'git', 'r2_url develop'),
                     # VerifyInput('Repo3-src', 'git', 'r3_url develop'),
                     # VerifyInput('Repo4-src', 'git', 'r4_url master'),
                     VerifyInput('Repo1-src', 'build', 'Repo1-inputs:update_inputs:Repo1-develop'),
                     VerifyInput('Repo2-src', 'build', 'Repo1-inputs:update_inputs:Repo2-develop'),
                     VerifyInput('Repo3-src', 'build', 'Repo1-inputs:update_inputs:Repo3-develop'),
                     VerifyInput('Repo4-src', 'build', 'Repo1-inputs:update_inputs:Repo4-master'),
                     VerifyInput('ghcver', 'string', ghcver),
                     VerifyInput('variant', 'string', 'branch=develop', contains=True),
                     VerifyInput('variant', 'string', 'strategy=HEADs', contains=True))
            for ghcver in ['ghc844', 'ghc865', 'ghc881']]),
        *tuple([
            VerifyBldCfg('develop.submodules-'+ghcver,
                         VerifyInput('Repo1-src', 'build', 'Repo1-inputs:update_inputs:Repo1-develop'),
                         VerifyInput('Repo2-src', 'build', 'Repo1-inputs:update_inputs:Repo2-r2_develop_head'),
                         VerifyInput('Repo3-src', 'build', 'Repo1-inputs:update_inputs:Repo3-r3_develop_head'),
                         VerifyInput('Repo4-src', 'build', 'Repo1-inputs:update_inputs:Repo4-r4_master_head'),
                         # VerifyInput('Repo1-src', 'git', 'r1_url develop'),
                         # VerifyInput('Repo2-src', 'git', 'r2_url r2_develop_head'),
                         # VerifyInput('Repo3-src', 'git', 'r3_url r3_develop_head'),
                         # VerifyInput('Repo4-src', 'git', 'r4_url r4_master_head'),
                         VerifyInput('ghcver', 'string', ghcver),
                         VerifyInput('variant', 'string', 'branch=develop', contains=True),
                         VerifyInput('variant', 'string', 'strategy=submodules', contains=True))
            for ghcver in ["ghc844", "ghc865", "ghc881"]]),
        *tuple([
            VerifyBldCfg('master.HEADs-' + ghcver,
                     VerifyInput('Repo1-src', 'build', 'Repo1-inputs:update_inputs:Repo1-master'),
                     VerifyInput('Repo2-src', 'build', 'Repo1-inputs:update_inputs:Repo2-master'),
                     VerifyInput('Repo3-src', 'build', 'Repo1-inputs:update_inputs:Repo3-master'),
                     VerifyInput('Repo4-src', 'build', 'Repo1-inputs:update_inputs:Repo4-master'),
                     # VerifyInput('Repo1-src', 'git', 'r1_url master'),
                     # VerifyInput('Repo2-src', 'git', 'r2_url master'),
                     # VerifyInput('Repo3-src', 'git', 'r3_url master'),
                     # VerifyInput('Repo4-src', 'git', 'r4_url master'),
                     VerifyInput('ghcver', 'string', ghcver),
                     VerifyInput('variant', 'string', 'branch=master', contains=True),
                     VerifyInput('variant', 'string', 'strategy=HEADs', contains=True))
            for ghcver in ['ghc844', 'ghc865', 'ghc881']]),
        *tuple([
            VerifyBldCfg('master.submodules-'+ghcver,
                         VerifyInput('Repo1-src', 'build', 'Repo1-inputs:update_inputs:Repo1-master'),
                         VerifyInput('Repo2-src', 'build', 'Repo1-inputs:update_inputs:Repo2-r2_master_head'),
                         VerifyInput('Repo3-src', 'build', 'Repo1-inputs:update_inputs:Repo3-r3_master_head^3'),
                         VerifyInput('Repo4-src', 'build', 'Repo1-inputs:update_inputs:Repo4-r4_master_head^1'),
                         # VerifyInput('Repo1-src', 'git', 'r1_url master'),
                         # VerifyInput('Repo2-src', 'git', 'r2_url r2_master_head'),
                         # VerifyInput('Repo3-src', 'git', 'r3_url r3_master_head^3'),
                         # VerifyInput('Repo4-src', 'git', 'r4_url r4_master_head^1'),
                         VerifyInput('ghcver', 'string', ghcver),
                         VerifyInput('variant', 'string', 'branch=master', contains=True),
                         VerifyInput('variant', 'string', 'strategy=submodules', contains=True))
            for ghcver in ["ghc844", "ghc865", "ghc881"]]),
        nixexprinput = 'Repo1-src',
        nixexprpath = './release.nix',
    ).verify(bldcfgs[None])

    verify_cfg_instructions(bldcfgs['Repo1.txt'])

    # The project config file is used as the declarative input for the
    # project itself.  The jobsets should be generated by the
    # gen_proj_jobsets.nix file generated in this same output set.
    VerifyProjectCfg(
        admin_nixpkgs,
        # The input which contains the primary expression
        VerifyInput('mkjobsets', 'path', bpath + '/hydra'),
        # This is the actual .jobsets, which is copied to the store by the nixexprpath
        VerifyInput('projcfg', 'build', 'Repo1-inputs:update_inputs:Repo1-cfg'),
        nixexprinput='mkjobsets',
        nixexprpath='gen_proj_jobsets.nix',
    ).verify(json.loads(bldcfgs['Repo1-hydra-project-config.json']))

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
                    bpath + '/hydra/Repo1_inpupd_jobsets.json'),
        # nixpkgs is needed for the gen_proj_jobsets.nix expression
        admin_nixpkgs,
        nixexprinput = 'inpupd_cfg',
        nixexprpath = 'gen_inpupd.nix',
    ).verify(json.loads(bldcfgs['Repo1-hydra-inputs-config.json']))

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
                     # Repo1-cfg-inp is KWQ? to re-run this if the input config changes??
                     VerifyInput('Repo1-cfg-inp', 'path', bpath + '/inp_example2'),

                     VerifyInput('Repo1-develop-src', 'git', 'r1_url develop'),
                     VerifyInput('Repo2-develop-src', 'git', 'r2_url develop'),
                     VerifyInput('Repo3-develop-src', 'git', 'r3_url develop'),
                     # VerifyInput('Repo4-develop-src', 'git', 'r4_url master'),

                     # VerifyInput('Repo1-develop-src', 'git', 'r1_url develop'),
                     VerifyInput('Repo2-r2_develop_head-src', 'git', 'r2_url r2_develop_head'),
                     VerifyInput('Repo3-r3_develop_head-src', 'git', 'r3_url r3_develop_head'),
                     VerifyInput('Repo4-r4_master_head-src', 'git', 'r4_url r4_master_head'),

                     VerifyInput('Repo1-master-src', 'git', 'r1_url master'),
                     VerifyInput('Repo2-master-src', 'git', 'r2_url master'),
                     VerifyInput('Repo3-master-src', 'git', 'r3_url master'),
                     VerifyInput('Repo4-master-src', 'git', 'r4_url master'),

                     # VerifyInput('Repo1-src', 'git', 'r1_url master'),
                     VerifyInput('Repo2-r2_master_head-src', 'git', 'r2_url r2_master_head'),
                     VerifyInput('Repo3-r3_master_head^3-src', 'git', 'r3_url r3_master_head^3'),
                     VerifyInput('Repo4-r4_master_head^1-src', 'git', 'r4_url r4_master_head^1'),

                     # nixpkgs is needed for the KWQ? expression
                     admin_nixpkgs,
        ),
        nixexprinput = 'realize_inputs',
        nixexprpath = 'Repo1-updinputs.nix',
    ).verify(bldcfgs[bpath + '/hydra/Repo1_inpupd_jobsets.json'])

    # The Repo1_updinputs.nix is the main nix expression that is
    # associated with the update_inputs jobset.
    updinp_expr = bldcfgs[bpath + '/hydra/Repo1-updinputs.nix']
    assert_in('Repo1-develop-src', updinp_expr)
    assert_in('Repo2-develop-src', updinp_expr)
    assert_in('Repo3-develop-src', updinp_expr)
    assert_in('Repo2-r2_develop_head-src', updinp_expr)
    assert_in('Repo3-r3_develop_head-src', updinp_expr)
    assert_in('Repo4-r4_master_head-src', updinp_expr)
    assert_in('Repo1-master-src', updinp_expr)
    assert_in('Repo2-master-src', updinp_expr)
    assert_in('Repo3-master-src', updinp_expr)
    assert_in('Repo4-master-src', updinp_expr)
    assert_in('Repo2-r2_master_head-src', updinp_expr)
    # n.b. the following 2 are invalid nix expression names; this is
    # OK since these are made up here and actual expressions are
    # computed from git branch names which should be constrained to
    # valid nix expression names as well.
    assert_in('Repo3-r3_master_head^3-src', updinp_expr)
    assert_in('Repo4-r4_master_head^1-src', updinp_expr)

# ----------------------------------------------------------------------

def verify_alt_proj_hydracfg(bpath, bldcfgs):

    assert None in bldcfgs
    other_bldcfgs = [x for x in bldcfgs.keys() if x is not None]
    # assert_eqlist(other_bldcfgs, [])
   : should not be unnamed!
    assert_eqlist(other_bldcfgs,
                  [ 'alt-proj-hydra-project-config.json',
                    'alt-proj-hydra-inputs-config.json',
                    os.path.join(bpath, 'hydra/gen_inpupd.nix'),
                    os.path.join(bpath, 'hydra/alt-proj_inpupd_jobsets.json'),
                    os.path.join(bpath, 'hydra/alt-proj-updinputs.nix'),
                    os.path.join(bpath, 'hydra/gen_proj_jobsets.nix'),
                    'alt-proj.txt',
                  ])

    VerifyJobsetDesc(
        VerifyInput('variant', 'string', 'strategy', contains=True),

        VerifyBldCfg('master.HEADs-ghc865',
                     # VerifyInput('Repo1-src', 'git', 'r1_url master'),
                     # VerifyInput('Repo2-src', 'git', 'r2_url master'),
                     # VerifyInput('Repo3-src', 'git', 'r3_url master'),
                     # VerifyInput('Repo4-src', 'git', 'r4_url master'),
                     # VerifyInput('RAdd1-src', 'git', 'ra1_url master'),
                     # VerifyInput('RAdd2-src', 'git', 'ra2_url master'),
                     VerifyInput('Repo1-src', 'build', 'alt-proj-inputs:update_inputs:Repo1-master'),
                     VerifyInput('Repo2-src', 'build', 'alt-proj-inputs:update_inputs:Repo2-master'),
                     VerifyInput('Repo3-src', 'build', 'alt-proj-inputs:update_inputs:Repo3-master'),
                     VerifyInput('Repo4-src', 'build', 'alt-proj-inputs:update_inputs:Repo4-master'),
                     VerifyInput('RAdd1-src', 'build', 'alt-proj-inputs:update_inputs:RAdd1-master'),
                     VerifyInput('RAdd2-src', 'build', 'alt-proj-inputs:update_inputs:RAdd2-master'),
                     VerifyInput('ghcver', 'string', 'ghc865'),
                     VerifyInput('variant', 'string', 'branch=master', contains=True),
                     VerifyInput('variant', 'string', 'strategy=HEADs', contains=True)),
        VerifyBldCfg('master.submodules-ghc865',
                     # VerifyInput('Repo1-src', 'git', 'r1_url master'),
                     # VerifyInput('Repo2-src', 'git', 'r2_url r2_master_head'),
                     # VerifyInput('Repo3-src', 'git', 'r3_url r3_master_head^3'),
                     # VerifyInput('Repo4-src', 'git', 'r4_url r4_master_head^1'),
                     # VerifyInput('RAdd1-src', 'git', 'ra1_url master'),
                     # VerifyInput('RAdd2-src', 'git', 'ra2_url master'),
                     VerifyInput('Repo1-src', 'build', 'alt-proj-inputs:update_inputs:Repo1-master'),
                     VerifyInput('Repo2-src', 'build', 'alt-proj-inputs:update_inputs:Repo2-r2_master_head'),
                     VerifyInput('Repo3-src', 'build', 'alt-proj-inputs:update_inputs:Repo3-r3_master_head^3'),
                     VerifyInput('Repo4-src', 'build', 'alt-proj-inputs:update_inputs:Repo4-r4_master_head^1'),
                     VerifyInput('RAdd1-src', 'build', 'alt-proj-inputs:update_inputs:RAdd1-master'),
                     VerifyInput('RAdd2-src', 'build', 'alt-proj-inputs:update_inputs:RAdd2-master'),
                     VerifyInput('ghcver', 'string', 'ghc865'),
                     VerifyInput('variant', 'string', 'branch=master', contains=True),
                     VerifyInput('variant', 'string', 'strategy=submodules', contains=True)),
        nixexprinput = 'Repo1-src',
        nixexprpath = './release.nix',
    ).verify(bldcfgs[None])


    verify_cfg_instructions(bldcfgs['alt-proj.txt'])

    # The project config file is used as the declarative input for the
    # project itself.  The jobsets should be generated by the
    # gen_proj_jobsets.nix file generated in this same output set.
    VerifyProjectCfg(
        admin_nixpkgs,
        # The input which contains the primary expression
        VerifyInput('mkjobsets', 'path', bpath + '/hydra'),
        # This is the actual .jobsets, which is copied to the store by the nixexprpath
        VerifyInput('projcfg', 'build', 'alt-proj-inputs:update_inputs:alt-proj-cfg'),
        nixexprinput='mkjobsets',
        nixexprpath='gen_proj_jobsets.nix',
    ).verify(json.loads(bldcfgs['alt-proj-hydra-project-config.json']))

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
                    bpath + '/hydra/alt-proj_inpupd_jobsets.json'),
        # nixpkgs is needed for the gen_proj_jobsets.nix expression
        admin_nixpkgs,
        nixexprinput = 'inpupd_cfg',
        nixexprpath = 'gen_inpupd.nix',
    ).verify(json.loads(bldcfgs['alt-proj-hydra-inputs-config.json']))

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
                     # Repo1-cfg-inp is KWQ? to re-run this if the input config changes??
                     VerifyInput('alt-proj-cfg-inp', 'path', bpath + '/inp_example2_add'),

                     VerifyInput('Repo1-master-src', 'git', 'r1_url master'),
                     VerifyInput('Repo2-master-src', 'git', 'r2_url master'),
                     VerifyInput('Repo3-master-src', 'git', 'r3_url master'),
                     VerifyInput('Repo4-master-src', 'git', 'r4_url master'),
                     VerifyInput('RAdd1-master-src', 'git', 'ra1_url master'),
                     VerifyInput('RAdd2-master-src', 'git', 'ra2_url master'),

                     VerifyInput('Repo2-r2_master_head-src', 'git', 'r2_url r2_master_head'),
                     VerifyInput('Repo3-r3_master_head^3-src', 'git', 'r3_url r3_master_head^3'),
                     VerifyInput('Repo4-r4_master_head^1-src', 'git', 'r4_url r4_master_head^1'),

                     # nixpkgs is needed for the KWQ? expression
                     admin_nixpkgs,
        ),
        nixexprinput = 'realize_inputs',
        nixexprpath = 'alt-proj-updinputs.nix',
    ).verify(bldcfgs[bpath + '/hydra/alt-proj_inpupd_jobsets.json'])

    # The Repo1_updinputs.nix is the main nix expression that is
    # associated with the update_inputs jobset.
    updinp_expr = bldcfgs[bpath + '/hydra/alt-proj-updinputs.nix']
    assert_in('Repo1-master-src', updinp_expr)
    assert_in('Repo2-master-src', updinp_expr)
    assert_in('Repo3-master-src', updinp_expr)
    assert_in('Repo4-master-src', updinp_expr)
    assert_in('RAdd1-master-src', updinp_expr)
    assert_in('RAdd2-master-src', updinp_expr)
    assert_in('Repo2-r2_master_head-src', updinp_expr)
    # n.b. the following 2 are invalid nix expression names; this is
    # OK since these are made up here and actual expressions are
    # computed from git branch names which should be constrained to
    # valid nix expression names as well.
    assert_in('Repo3-r3_master_head^3-src', updinp_expr)
    assert_in('Repo4-r4_master_head^1-src', updinp_expr)


def verify_cfg_instructions(instrs):
    # Should be some instructions... don't bother to check their details
    assert instrs
