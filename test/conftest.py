"Support functionality for running the tests."

import pytest
from datetime import datetime, timedelta
from thespian.actors import *
from Briareus.Types import *
import Briareus.AnaRep.Operations as AnaRep
import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
import Briareus.BuildSys.Hydra as BldSys
from Briareus.BuildSys import buildcfg_name
import Briareus.BCGen.Operations as BCGen
from Briareus.Logic.Evaluation import (Fact, DeclareFact)
import Briareus.hh as hh


@pytest.fixture(scope="module")
def actor_system():
    asys = ActorSystem('simpleSystemBase', transientUnique=True)
    yield asys
    asys.shutdown()

@pytest.fixture(scope="module")
def generated_repo_info(actor_system, request):
    gitinfo = actor_system.createActor(request.module.gitactor, globalName="GetGitInfo")
    if hasattr(request.module, 'gitactor_updates'):
        for req,resp in request.module.gitactor_updates:
            r = actor_system.ask(gitinfo, req, timedelta(seconds=1))
            assert r == resp
    return BInput.input_desc_and_VCS_info(request.module.input_spec,
                                          verbose=True,
                                          actor_system=actor_system)

@pytest.fixture(scope="module")
def generated_facts(actor_system, generated_repo_info):
    "Generate facts from a single configuration"
    inp, repo_info = generated_repo_info
    gen = Generator.Generator(actor_system=actor_system, verbose=True)
    (rtype, facts) = gen.generate_build_configs(inp, repo_info, up_to="facts")
    assert rtype == "facts"
    return (sorted([F for F in facts if isinstance(F, DeclareFact)]) +
            sorted([F for F in facts if isinstance(F, Fact)]))

@pytest.fixture(scope="module")
def generated_inp_config_facts(actor_system, testing_dir, inp_configs):
    "Generate facts from a list of (gitactor, outcfg_fname, InpConfig) inputs"
    params = hh.Params(verbose=True, up_to=hh.UpTo("facts"),
                       report_file=testing_dir.join("testreport.hhr"))
    prev_result = RunContext(actor_system=actor_system)
    result = []
    for git, outf, inpcfg in inp_configs:
        # Generate canned info instead of actually doing git operations
        gitActor = actor_system.createActor(git, globalName="GetGitInfo")
        with open(inpcfg.hhd, 'r') as inpf:
            result.extend(hh.run_hh_gen(params, inpcfg, inpf.read(), None, prev_result))
        actor_system.ask(gitActor, ActorExitRequest(), 1)
        actor_system.ask(actor_system.createActor(git, globalName="GatherRepoInfo"), ActorExitRequest(), 1)
    return result

@pytest.fixture(scope="module")
def generated_bldconfigs(actor_system, generated_repo_info):
    "Generate build configs from a single configuration"
    inp, repo_info = generated_repo_info
    gen = Generator.Generator(actor_system=actor_system, verbose=True)
    (rtype, cfgs) = gen.generate_build_configs(inp, repo_info)
    assert rtype == "build_configs"
    return cfgs

@pytest.fixture(scope="module")
def generated_inp_config_bldconfigs(actor_system, testing_dir, inp_configs, request):
    "Return bldconfigs from a list of (gitactor, outcfg_fname, InpConfig) inputs"
    starttime = datetime.now()
    params = hh.Params(verbose=True, up_to=hh.UpTo("builder_configs"),
                       report_file=testing_dir.join("testreport_bldcfgs.hhr"))
    result = RunContext(actor_system=actor_system)
    for git, outf, inpcfg in inp_configs:
        # Generate canned info instead of actually doing git operations
        gitActor = actor_system.createActor(git, globalName="GetGitInfo")
        with open(inpcfg.hhd, 'r') as inpf:
            result = hh.run_hh_gen(params, inpcfg, inpf.read(), None, result)
        actor_system.ask(gitActor, ActorExitRequest(), 1)
        actor_system.ask(actor_system.createActor(git, globalName="GatherRepoInfo"), ActorExitRequest(), 1)
    endtime = datetime.now()
    # This should be a proper test: checks the amount of time to run run the logic process.
    if hasattr(request.module, 'build_output_time_budget'):
        assert endtime - starttime < request.module.build_output_time_budget
    return result


@pytest.fixture(scope="module")
def generated_hydra_builder_output(actor_system, generated_repo_info, request):
    starttime = datetime.now()
    input_desc, repo_info = generated_repo_info
    builder = BldSys.HydraBuilder(None)
    bcgen = BCGen.BCGen(builder, actor_system=actor_system, verbose=True)
    output = bcgen.generate(input_desc, repo_info)
    endtime = datetime.now()
    # This should be a proper test: checks the amount of time to run run the logic process.
    if hasattr(request.module, 'build_output_time_budget'):
        assert endtime - starttime < request.module.build_output_time_budget
    return output

@pytest.fixture(scope="module")
def generate_hydra_results(actor_system, generated_repo_info, generated_hydra_builder_output, request):
    def _ghr(build_results, prior, reporting_logic_defs=''):
        starttime = datetime.now()
        inp_desc, repo_info = generated_repo_info
        builder_cfgs, build_cfgs = generated_hydra_builder_output
        anarep = AnaRep.AnaRep(verbose=True, actor_system=actor_system)
        # n.b. the name values for build_results come from
        # buildcfg_name, which is revealed by this print loop.
        builder = BldSys.HydraBuilder(None)
        for each in build_cfgs.cfg_build_configs:
            print(buildcfg_name(each))
        builder._build_results = build_results
        rcontext = RunContext(actor_system,
                              [AnaRep.ResultSet(builder, inp_desc, repo_info, build_cfgs)])
        report = anarep.report_on(rcontext,
                                  prior,
                                  reporting_logic_defs=reporting_logic_defs)
        assert report[0] == 'report'
        endtime = datetime.now()
        # This should be a proper test: checks the amount of time to run run the logic process.
        if hasattr(request.module, 'analysis_time_budget'):
            assert endtime - starttime < request.module.analysis_time_budget
        return (builder_cfgs, report[1])
    return _ghr
