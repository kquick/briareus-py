# Base definitions for a Builder

class Builder(object):
    def __init__(self, conf_file, builder_url=None):
        self._conf_file = conf_file
        self._builder_url = builder_url
