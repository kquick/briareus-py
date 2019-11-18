import Briareus.AnaRep.Operations as AnaRep
import Briareus.BCGen.Operations as BCGen
from Briareus.Types import (BldConfig, BldRepoRev, BldVariable,
                            ProjectSummary, StatusReport, VarFailure)
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from thespian.actors import *
from git_example1 import GitExample1
import json
import pytest
from test_example import input_spec


@pytest.fixture(scope="module")
def example_hydra_results():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        # Generate canned info instead of actually doing git operations
        asys.createActor(GitExample1, globalName="GetGitInfo")
        inp_desc, repo_info = BInput.input_desc_and_VCS_info(input_spec,
                                                             actor_system=asys,
                                                             verbose=True)
        builder = BldSys.HydraBuilder(None)
        bcgen = BCGen.BCGen(builder, actor_system=asys, verbose=True)
        config_results = bcgen.generate(inp_desc, repo_info)
        builder_cfgs, build_cfgs = config_results
        anarep = AnaRep.AnaRep(verbose=True, actor_system=asys)
        # n.b. the name values for build_results come from
        # builder._jobset_name, which is revealed by this print loop.
        for each in build_cfgs.cfg_build_configs:
            print(builder._jobset_name(each))
        builder._build_results = [
            { "name": n,
              "nrtotal" : 10,
              "nrsucceeded": 8 if '-clang-' in n else 10,
              "nrfailed": 2 if '-clang-' in n else 0,
              "nrscheduled": 0,
              "haserrormsg": False,
            }
            for n in [
                    "PR1-PR11-PR111-PR1111-blah.HEADs-clang-ghc844",
                    "PR1-PR11-PR111-PR1111-blah.HEADs-gnucc-ghc844",
                    "PR1-PR11-PR111-PR1111-blah.HEADs-clang-ghc865",
                    "PR1-PR11-PR111-PR1111-blah.HEADs-gnucc-ghc865",
                    "PR1-PR11-PR111-PR1111-blah.HEADs-clang-ghc881",
                    "PR1-PR11-PR111-PR1111-blah.HEADs-gnucc-ghc881",
                    "PR1-PR111-blah.submodules-clang-ghc844",
                    "PR1-PR111-blah.submodules-gnucc-ghc844",
                    "PR1-PR111-blah.submodules-clang-ghc865",
                    "PR1-PR111-blah.submodules-gnucc-ghc865",
                    "PR1-PR111-blah.submodules-clang-ghc881",
                    "PR1-PR111-blah.submodules-gnucc-ghc881",
                    "PR23-PR8192-bugfix9.HEADs-clang-ghc844",
                    "PR23-PR8192-bugfix9.HEADs-gnucc-ghc844",
                    "PR23-PR8192-bugfix9.HEADs-clang-ghc865",
                    "PR23-PR8192-bugfix9.HEADs-gnucc-ghc865",
                    "PR23-PR8192-bugfix9.HEADs-clang-ghc881",
                    "PR23-PR8192-bugfix9.HEADs-gnucc-ghc881",
                    "PR23-PR8192-bugfix9.submodules-clang-ghc844",
                    "PR23-PR8192-bugfix9.submodules-gnucc-ghc844",
                    "PR23-PR8192-bugfix9.submodules-clang-ghc865",
                    "PR23-PR8192-bugfix9.submodules-gnucc-ghc865",
                    "PR23-PR8192-bugfix9.submodules-clang-ghc881",
                    "PR23-PR8192-bugfix9.submodules-gnucc-ghc881",
                    "dev.HEADs-clang-ghc844",
                    "dev.HEADs-gnucc-ghc844",
                    "dev.HEADs-clang-ghc865",
                    "dev.HEADs-gnucc-ghc865",
                    "dev.HEADs-clang-ghc881",
                    "dev.HEADs-gnucc-ghc881",
                    "dev.submodules-clang-ghc844",
                    "dev.submodules-gnucc-ghc844",
                    "dev.submodules-clang-ghc865",
                    "dev.submodules-gnucc-ghc865",
                    "dev.submodules-clang-ghc881",
                    "dev.submodules-gnucc-ghc881",
                    "feat1.HEADs-clang-ghc844",
                    "feat1.HEADs-gnucc-ghc844",
                    "feat1.HEADs-clang-ghc865",
                    "feat1.HEADs-gnucc-ghc865",
                    "feat1.HEADs-clang-ghc881",
                    "feat1.HEADs-gnucc-ghc881",
                    "feat1.submodules-clang-ghc844",
                    "feat1.submodules-gnucc-ghc844",
                    "feat1.submodules-clang-ghc865",
                    "feat1.submodules-gnucc-ghc865",
                    "feat1.submodules-clang-ghc881",
                    "feat1.submodules-gnucc-ghc881",
                    "master.HEADs-clang-ghc844",
                    "master.HEADs-gnucc-ghc844",
                    "master.HEADs-clang-ghc865",
                    "master.HEADs-gnucc-ghc865",
                    "master.HEADs-clang-ghc881",
                    "master.HEADs-gnucc-ghc881",
                    "master.submodules-clang-ghc844",
                    "master.submodules-gnucc-ghc844",
                    "master.submodules-clang-ghc865",
                    "master.submodules-gnucc-ghc865",
                    "master.submodules-clang-ghc881",
                    "master.submodules-gnucc-ghc881",
            ]
        ]
        prior = [
            StatusReport(status='initial_success', project='R1',
                         strategy="submodules", branchtype="regular", branch="master",
                         buildname='master.submodules-gnucc-ghc844',
                         bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue='ghc844'),
                                  BldVariable(projrepo='R1', varname='c_compiler', varvalue='gnucc'),
                         ]),
            StatusReport(status='failed', project='R1',
                         strategy="HEADs", branchtype="regular", branch="master",
                         buildname='master.HEADs-gnucc-ghc865',
                         bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue='ghc865'),
                                  BldVariable(projrepo='R1', varname='c_compiler', varvalue='gnucc'),
                         ]),
            StatusReport(status='succeeded', project='R1',
                         strategy="HEADs", branchtype="regular", branch="master",
                         buildname='master.HEADs-gnucc-ghc844',
                         bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue='ghc844'),
                                  BldVariable(projrepo='R1', varname='c_compiler', varvalue='gnucc'),
                         ]),
        ]
        report = anarep.report_on([AnaRep.ResultSet(builder, inp_desc, repo_info, build_cfgs)], prior)
        assert report[0] == 'report'
        yield (builder_cfgs, report[1])
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()


def test_example_report_summary(example_hydra_results):
    bldcfgs, reps = example_hydra_results

    for each in reps:
        print('')
        print(each)
    print('')
    print(len(reps))
    assert ProjectSummary(project_name='R1',
                          bldcfg_count=60, subrepo_count=4, pullreq_count=6) in reps

def test_example_report_status1(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for a single entry
    assert StatusReport(status='failed', project='R1',
                        strategy="HEADs", branchtype="pullreq", branch="blah",
                        buildname='PR1-PR11-PR111-PR1111-blah.HEADs-clang-ghc844',
                        bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue='ghc844'),
                                 BldVariable(projrepo='R1', varname='c_compiler', varvalue='clang'),
                        ]) in reps

CS = [ 'clang', 'gnucc' ]
GS = [ 'ghc844', 'ghc865', 'ghc881' ]
SS = [ 'HEADs', 'submodules' ]
BS = [ 'PR23-PR8192-bugfix9', "feat1", "master", "dev",]

def test_example_report_statusMany(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for all entries that should be present
    for C in CS:
        for G in GS:
            for S in SS:
                for B in BS:
                    r = StatusReport(
                        status=('succeeded'
                                if C == 'gnucc' and G == 'ghc844' and S == 'submodules' and B == 'master'
                                else 'fixed'
                                if C == 'gnucc' and G == 'ghc865' and S == 'HEADs' and B == 'master'
                                else 'succeeded'
                                if C == 'gnucc' and G == 'ghc844' and S == 'HEADs' and B == 'master'
                                else 'failed'
                                if C == 'clang'
                                else 'initial_success'),
                        project='R1',
                        strategy=S,
                        branchtype="pullreq" if B.startswith('PR') else "regular",
                        branch=B.split('-')[-1] if B.startswith('PR') else B,
                        buildname='-'.join(['.'.join([B,S]),C,G]),
                        bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue=G),
                                 BldVariable(projrepo='R1', varname='c_compiler', varvalue=C),
                        ])
                    assert r in reps

def test_example_report_status2(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    for each in reps:
        print('')
        print(each)
    # Check for all entries that should be present
    for C in CS:
        for G in GS:
            S = 'submodules'
            B = 'PR1-PR111-blah'
            r = StatusReport(
                status=('failed'
                        if C == 'clang'
                        else 'initial_success'),
                project='R1',
                strategy=S,
                branchtype="pullreq" if B.startswith('PR') else "regular",
                branch=B.split('-')[-1] if B.startswith('PR') else B,
                buildname='-'.join(['.'.join([B,S]),C,G]),
                bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue=G),
                         BldVariable(projrepo='R1', varname='c_compiler', varvalue=C),
                ])
            assert r in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for all entries that should be present
    for C in CS:
        for G in GS:
            S = 'HEADs'
            B = 'PR1-PR11-PR111-PR1111-blah'
            r = StatusReport(
                status=('failed'
                        if C == 'clang'
                        else 'initial_success'),
                project='R1',
                strategy=S,
                branchtype="pullreq" if B.startswith('PR') else "regular",
                branch=B.split('-')[-1] if B.startswith('PR') else B,
                buildname='-'.join(['.'.join([B,S]),C,G]),
                bldvars=[BldVariable(projrepo='R1', varname='ghcver', varvalue=G),
                         BldVariable(projrepo='R1', varname='c_compiler', varvalue=C),
                ])
            assert r in reps

def test_example_report_varfailure(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert VarFailure('R1', 'c_compiler', 'clang') in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Verify that there are no unexpected additional entries
    nrscheduled = 0
    prfailing = 4
    prsuccess = 0
    additional_bldcfgs = 1 # status2 + status3
    expected = ((len(CS) * len(GS) * len(SS) * (len(BS)+additional_bldcfgs)) +
                len(['ProjectSummary', 'VarFailure'])
                + prfailing + prsuccess - nrscheduled)
    assert expected == len(reps)
