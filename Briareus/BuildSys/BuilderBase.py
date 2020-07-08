from typing import Dict, Optional, Union
from Briareus.Types import BuilderResult, BldConfig
from Briareus.BuildSys_API import BuilderConfigsTy, BuilderURL


# Base definitions for a Builder

class Builder(object):
    def __init__(self, conf_file: Optional[str], builder_url: BuilderURL = None) -> None:
        self._conf_file = conf_file
        self._builder_url = builder_url

    def output_build_configurations(self, input_desc,
                                    bldcfgs,
                                    bldcfg_fname: str = None,
                                    verbose: bool = False) -> BuilderConfigsTy:
        """Given an input description and the set of build configurations
           generated from the BCGen logic, return the builder-specific
           configuration of those build configurations, along with any
           auxiliary files as a dictionary, where the key is the
           filename and the value is the contents; the key should be
           None for the primary output file, which is named in the
           input specification.

        """
        raise RuntimeError("This method must be implemented"
                           " in a Builder-specific subclass")


    def get_build_result(self, bldcfg: BldConfig) -> Union[str, BuilderResult]:
        """Queries the builder for the result of the described BldConfig"""
        raise RuntimeError("This method must be implemented"
                           " in a Builder-specific subclass")


    def get_project_url(self, project: str) -> Optional[BuilderURL]:
        """Returns the URL for the builder's build summary for a specific
           project."""
        raise RuntimeError("This method must be implemented"
                           " in a Builder-specific subclass")
