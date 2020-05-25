import Briareus.Input.Operations as BInput
import Briareus.BCGen.Generator as Generator
from test_example import expected_repo_info
import pytest

# Note: mixes mismatched expected_repo_info and input_spec
input_spec = open('test/inp_example4').read()

def test_incorrect_input_facts_attempt(actor_system, generated_repo_info):
    inp, repo_info = generated_repo_info
    gen = Generator.Generator(actor_system = actor_system)
    with pytest.raises(RuntimeError) as excinfo:
        (rtype, facts) = gen.generate_build_configs(inp, repo_info, up_to="facts")
    assert "The following repos have no available branches: ['R10']" in str(excinfo.value)
