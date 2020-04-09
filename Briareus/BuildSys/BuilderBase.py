# Base definitions for a Builder

class Builder(object):
    def __init__(self, conf_file, builder_url=None):
        self._conf_file = conf_file
        self._builder_url = builder_url

    def output_build_configurations(self, input_desc,
                                    bldcfgs, bldcfg_fname=None,
                                    verbose=False):
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
