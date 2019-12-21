import Briareus.BCGen.Operations as BCGen
from Briareus.Types import (BldConfig, BldRepoRev, BldVariable,
                            ProjectSummary, CompletelyFailing,
                            StatusReport, VarFailure)
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
import Briareus.hh as hh
from thespian.actors import *
from git_example1 import GitExample1
from git_example2 import GitExample2
import git_exampledups as GitDups
import json
import os
import pytest
import test_example as texample
import test_example_results as texres
import test_facts as texfacts
import test_example3 as tex3
import test_exampledups as tdups
from datetime import datetime, timedelta


# This test combines test_example, test_example3, and test_exampledups
# using the input configs file to specify multiple projects.

@pytest.fixture(scope="session")
def testing_dir(tmpdir_factory):
    return tmpdir_factory.mktemp("ex1_ex3_dups")

@pytest.fixture(scope="session")
def example_hhd(testing_dir):
    hhd = testing_dir.join("example.hdd")
    hhd.write(texample.input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="session")
def example3_hhd(testing_dir):
    hhd = testing_dir.join("example3.hdd")
    hhd.write(tex3.input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="session")
def dup_hhd(testing_dir):
    hhd = testing_dir.join("dups.hdd")
    hhd.write(tdups.input_spec)
    yield str(hhd)
    print('os.remove(%s)' % hhd)

@pytest.fixture(scope="module")
def inp_configs(testing_dir, example_hhd, example3_hhd, dup_hhd):
    outfile_example = testing_dir.join("ex.hhc")
    outfile_example3 = testing_dir.join("ex3.hhc")
    outfile_dup = testing_dir.join("dup.hhc")
    return [
        (GitExample1, outfile_example,
         hh.InpConfig(hhd=example_hhd,
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_example,
         ),
        ),
        (GitExample1, outfile_example3,
         hh.InpConfig(hhd=example3_hhd,
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_example3,
         ),
        ),
        (GitDups.GitExample, outfile_dup,
         hh.InpConfig(hhd=dup_hhd,
                      builder_type="hydra",
                      # builder_conf="
                      output_file=outfile_dup,
         ),
        ),
    ]


def test_example_facts(testing_dir, inp_configs):
    asys = ActorSystem(transientUnique=True)
    try:
        params = hh.Params(verbose=True, up_to=hh.UpTo("facts"),
                           report_file=testing_dir.join("ex1_ex3_dups.hhr"))
        prev_result = hh.GenResult(actor_system=asys)
        result = []
        for git, outf, inpcfg in inp_configs:
            # Generate canned info instead of actually doing git operations
            gitActor = asys.createActor(git, globalName="GetGitInfo")
            with open(inpcfg.hhd, 'r') as inpf:
                result.extend(hh.run_hh_gen(params, inpcfg, inpf.read(), None, prev_result))
            asys.ask(gitActor, ActorExitRequest(), 1)
            asys.ask(asys.createActor(git, globalName="GatherRepoInfo"), ActorExitRequest(), 1)
        assert expected_facts == sorted(map(str, result))
    finally:
        asys.shutdown()

# Note: facts are currently duplicated for repos shared by multiple projects... this shouldn't be a huge issue.

expected_facts = sorted(filter(None,
                               texfacts.expected_facts +
                               tex3.expected_facts +
                               tdups.expected_facts))


# ----------------------------------------------------------------------
# The build configurations do not need to be exhaustively tested: the
# GenResults returned by example_internal_bldconfigs is simply an
# array of the individual configurations already tested by
# test_example.py, test_example3.py, and test_exampledups.py.
#
# The tests here simply verify the identical number of configurations:
# if these tests here fail, the corresponding test file above should
# be failing as well and the tests there should be addressed first
# (these tests will probably start succeeding automatically when the
# above are addressed).

@pytest.fixture(scope="module")
def example_internal_bldconfigs(testing_dir, inp_configs):
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    try:
        starttime = datetime.now()
        params = hh.Params(verbose=True, up_to=hh.UpTo("builder_configs"),
                           report_file=testing_dir.join("ex1_ex3_dups.hhr"))
        result = hh.GenResult(actor_system=asys)
        for git, outf, inpcfg in inp_configs:
            # Generate canned info instead of actually doing git operations
            gitActor = asys.createActor(git, globalName="GetGitInfo")
            with open(inpcfg.hhd, 'r') as inpf:
                result = hh.run_hh_gen(params, inpcfg, inpf.read(), None, result)
            asys.ask(gitActor, ActorExitRequest(), 1)
            asys.ask(asys.createActor(git, globalName="GatherRepoInfo"), ActorExitRequest(), 1)
        endtime = datetime.now()
        # This should be a proper test: checks the amount of time to run run the logic process.
        assert endtime - starttime < timedelta(seconds=2, milliseconds=500)  # avg 1.56s
        yield result
        asys.shutdown()
        asys = None
    finally:
        if asys:
            asys.shutdown()

ex_GS = texample.GS
ex_CS = texample.CS
ex_top_level = texample.top_level

ex3_GS = tex3.GS
ex3_top_level = tex3.top_level

dups_GS = tdups.GS
dups_top_level = tdups.top_level

def test_example_bldcfg_count(example_internal_bldconfigs):
    # Uncomment this to see all hydra jobnames
    # for each in example_internal_bldconfigs.result_sets:
    #     print([R for R in each.inp_desc.RL if R.project_repo][0].repo_name)
    #     for cfgnum, eachcfg in enumerate(each.build_cfgs.cfg_build_configs):
    #         print('',cfgnum,each.builder._jobset_name(eachcfg))
    #     print('')
    ex_results = [ Res for Res in example_internal_bldconfigs.result_sets
                   if any([R for R in Res.inp_desc.RL if R.project_repo and R.repo_name == 'R1' ])][0]
    assert len(ex_GS) * len(ex_CS) * len(ex_top_level) == len(set(ex_results.build_cfgs.cfg_build_configs))

def test_example3_bldcfg_count(example_internal_bldconfigs):
    ex3_results = [ Res for Res in example_internal_bldconfigs.result_sets
                    if any([R for R in Res.inp_desc.RL if R.project_repo and R.repo_name == 'R10' ])][0]
    assert len(ex3_GS) * len(ex3_top_level) == len(set(ex3_results.build_cfgs.cfg_build_configs))

def test_exampledups_bldcfg_count(example_internal_bldconfigs):
    dup_results = [ Res for Res in example_internal_bldconfigs.result_sets
                    if any([R for R in Res.inp_desc.RL if R.project_repo and R.repo_name == 'Repo1' ])][0]
    assert len(dups_GS) * len(dups_top_level) == len(set(dup_results.build_cfgs.cfg_build_configs))

# ----------------------------------------------------------------------
# AnaRep tests
#
# n.b to see builder (hydra) jobset names, see test_example_bldcfg_count above

@pytest.fixture(scope="module")
def example_empty_report(testing_dir, example_internal_bldconfigs):
    params = hh.Params(verbose=True, up_to=None,
                       report_file=testing_dir.join("ex1_ex3_dups.hhr"))
    for each in example_internal_bldconfigs.result_sets:
        each.builder._build_results = []
    return hh.run_hh_report(params, example_internal_bldconfigs, [])


def test_example_empty_report_summary(example_empty_report):
    reps = example_empty_report

    for each in reps:
        print('')
        print(each)

    assert ProjectSummary(project_name='R1+R10+Repo1',
                          bldcfg_count=92, subrepo_count=6, pullreq_count=15) in reps

def test_example_empty_report_complete_failures(example_empty_report):
    reps = example_empty_report

    assert CompletelyFailing(project='R1') in reps
    assert CompletelyFailing(project='R10') in reps
    assert CompletelyFailing(project='Repo1') in reps


# ----------------------------------------

@pytest.fixture(scope="module")
def example_report(testing_dir, example_internal_bldconfigs):
    params = hh.Params(verbose=True, up_to=None,
                       report_file=testing_dir.join("ex1_ex3_dups.hhr"))
    for each in example_internal_bldconfigs.result_sets:
        each.builder._build_results = {
            "R1": texres.hydra_results,
            "R10": [],
            "Repo1": tdups.hydra_results,
        }[[R.repo_name for R in each.inp_desc.RL if R.project_repo][0]]
    starttime = datetime.now()
    rep = hh.run_hh_report(params, example_internal_bldconfigs,
                           tdups.prior + texres.prior)
    endtime = datetime.now()
    # This should be a proper test: checks the amount of time to run run the logic process.
    assert endtime - starttime < timedelta(seconds=1, milliseconds=500)  # avg 1.02s
    return rep

def test_example_report_summary(example_report):
    reps = example_report

    for each in reps:
        print('')
        print(each)

    assert ProjectSummary(project_name='R1+R10+Repo1',
                          bldcfg_count=92, subrepo_count=6, pullreq_count=15) in reps

def test_example_report_varfailures(example_report):
    reps = example_report
    assert VarFailure(projrepo='R1', varname='c_compiler', varvalue='clang') in reps
    assert VarFailure(projrepo='Repo1', varname='ghcver', varvalue='ghc881') in reps
