import Briareus.Input.Parser as Parser
from Briareus.VCS.ManagedRepo import gather_repo_info


def input_desc_and_VCS_info(input_spec,
                            repo_auth=None,
                            verbose=False,
                            actor_system=None):
    """Called with the input_spec as a string and parses that
       specification to get the input description, and then gets VCS
       information for the repositories mentioned in the input
       description.  Throws exceptions on errors.
    """
    parser = Parser.BISParser(verbose=verbose)
    input_desc = parser.parse(input_spec)
    # Identify all of the repos by parsing the input specification,
    # removing duplicates, and adding in any specified in the
    # gitmodules of the project repo.  Then actively gather
    # information from the repositories.
    repo_info = gather_repo_info(input_desc.RL, input_desc.BL,
                                 repo_auth=repo_auth,
                                 actor_system=actor_system)
    return (input_desc, repo_info)
