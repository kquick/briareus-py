# Module providing parsing of Briareus input specifications

import sys
import Briareus.Input.Description as Desc


class BISParser(object):
    "Parser for the Briareus Input Specification"

    def __init__(self, verbose=False):
        self.verbose = verbose

    def parse(self, input_spec):
        x = eval(input_spec)
        # print(x)
        # print(x['Repos'])
        # print(x['Branches'])

        bad_repospecs = [(n,r) for n,r in enumerate(x['Repos'])
                         if not isinstance(r, tuple)]
        if bad_repospecs:
            for n,r in bad_repospecs:
                print("Invalid specification for repo #%d: %s" % (n, r),
                      file=sys.stderr)
            raise TypeError("Invalid repo types, expected tuples: %s"
                            % str(bad_repospecs))

        repos = set([Desc.RepoDesc(*r, project_repo=(n == 0))
                     for n,r in enumerate(x['Repos'])])
        pname = x.get('Name', [r.repo_name for r in repos if r.project_repo][0])
        branches = set([Desc.BranchDesc(b) for b in x.get('Branches', [])])
        repo_locs = set([Desc.RepoLoc(*r) for r in x.get('RepoLoc', list())])
        bldvars = [ Desc.VariableDesc(n,v)
                    for n,v in x.get('Variables',dict()).items() ]
        reporting = x.get('Reporting', dict())
        r = Desc.InputDesc(RL=sorted(list(repos)),
                           BL=sorted(list(branches)),
                           VAR=bldvars,
                           RX=sorted(list(repo_locs)),
                           REP=reporting,
                           PNAME=pname)
        if self.verbose: print('Input description: ', r)
        return r
