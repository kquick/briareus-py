# Data representation of build configurations

import attr

@attr.s(frozen=True)
class BldConfig(object):
    branchtype = attr.ib(default="regular")
    branchname = attr.ib(default="master")
    strategy   = attr.ib(default="main")
    blds       = attr.ib(factory=frozenset, converter=frozenset)  # list of BldRepoRev
    bldvars    = attr.ib(factory=frozenset, converter=frozenset)  # list of BldVariable

@attr.s(frozen=True)
class BldRepoRev(object):
    reponame = attr.ib()
    repover  = attr.ib()
      # srcident identifies the source loc in the Logic rules and is
      # used for debugging the ruleset.  As a result, it is ignored
      # for comparisons since a specific repo revision build
      # instruction is effective in the builder regardless of which
      # logic statement generated it.
    srcident = attr.ib(default="unk", cmp=False)

@attr.s(frozen=True)
class BldVariable(object):
    varname = attr.ib()
    varvalue = attr.ib()


# ----------------------------------------------------------------------

@attr.s(frozen=True)
class BuildResult(object):
    bldconfig = attr.ib()  # BldConfig
    results = attr.ib()    # BuilderResult

@attr.s(frozen=True)
class BuilderResult(object):
    buildname   = attr.ib()  # string name of build on builder
    nrtotal     = attr.ib()  # int
    nrsucceeded = attr.ib()  # int
    nrfailed    = attr.ib()  # int
    nrscheduled = attr.ib()  # int
    cfgerror    = attr.ib()  # bool



# ----------------------------------------------------------------------
# The Prolog output is currently interpreted via an "eval({output})"
# operation, so define some terms to re-ify the eval'd string into
# Python description.

logic_result_expr = {
    "pullreq": "pullreq",
    "submodules": "submodules",
    "heads": "HEADs",
    "regular": "regular",
    "main": "main",
    "bldcfg": lambda *args: BldConfig(*args),
    "bld": lambda *args: BldRepoRev(*args),
    "brr": lambda n: n,
    "var": lambda *args: BldVariable(*args),
}
