# Module providing parsing of Briareus input specifications

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
        repos = set([Desc.RepoDesc(*r, project_repo=(n == 0)) for n,r in enumerate(x['Repos'])])
        branches = set([Desc.BranchDesc(b) for b in x['Branches']])
        bldvars = [ Desc.VariableDesc(n,v)
                    for n,v in x.get('Variables',dict()).items() ]
        r = Desc.InputDesc(sorted(list(repos)), sorted(list(branches)), bldvars)
        if self.verbose: print('Input description: ', r)
        return r
