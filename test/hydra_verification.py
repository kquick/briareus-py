import json
import pytest
from helpers import *


class VerifyJSONFile(object):
    def verify(self, filedata):
        self.pythondata = json.loads(filedata)
        # Verification at this point is that json.loads did not fail

class VerifyJobsetDesc(VerifyJSONFile):
    def __init__(self, *inputs, **kw):
        self.keyvals = kw
        self.inputs_for_every_cfg = []
        self.bldcfgs = []
        for each in inputs:
            if isinstance(each, VerifyInput):
                self.inputs_for_every_cfg.append(each)
            elif isinstance(each, VerifyBldCfg):
                each.set_nixexprpath(self.keyvals['nixexprpath'])
                each.set_nixexprinput(self.keyvals['nixexprinput'])
                self.bldcfgs.append(each)
            else:
                raise RuntimeError("TBD: VerifyJobsetDesc input %s", type(each))
        for inp in self.inputs_for_every_cfg:
            for bcfg in self.bldcfgs:
                bcfg.add_input(inp)

    def verify(self, filedata):
        print(self.__class__.__name__, 'verify')
        super(VerifyJobsetDesc, self).verify(filedata)
        cfg = self.pythondata

        assert_eqlist(cfg.keys(),
                      [c.cfgname for c in self.bldcfgs])

        for bcfg in cfg.keys():
            for vbc in self.bldcfgs:
                if bcfg == vbc.cfgname:
                    vbc.verify(cfg[bcfg])
                    break


class VerifyBldCfg(object):
    def __init__(self, cfgname, *inputs, nixexprpath=None, nixexprinput=None):
        self.cfgname = cfgname
        self.nixexprpath = nixexprpath
        self.nixexprinput = nixexprinput
        self.inputs = []
        for each in inputs:
            if isinstance(each, VerifyInput):
                self.inputs.append(each)
            else:
                raise RuntimeError("TBD: VerifyBldCfg input %s", type(each))

    def set_nixexprpath(self, path):
        self.nixexprpath = path

    def set_nixexprinput(self, inp):
        self.nixexprinput = inp

    def add_input(self, inp):
        self.inputs.append(inp)

    valid_bldcfg_fields = [
        'checkinterval',
        'description',
        'emailoverride',
        'enabled',
        'enableemail',
        'hidden',
        'inputs',
        'keepnr',
        'nixexprinput',
        'nixexprpath',
        'schedulingshares',
    ]

    def verify(self, bldcfg):
        print(self.__class__.__name__, 'verify', self.cfgname)

        assert_eqlist(bldcfg.keys(), self.valid_bldcfg_fields)

        assert_eq(bldcfg['nixexprinput'], self.nixexprinput)
        assert_eq(bldcfg['nixexprpath'], self.nixexprpath)
        assert_eq(bldcfg['enabled'], 1)

        assert_eqlist(bldcfg['inputs'].keys(),
                      set([i.inpName for i in self.inputs]))

        for inp in self.inputs:
            inp.verify(bldcfg['inputs'][inp.inpName])


class VerifyProjectCfg(VerifyBldCfg):
    def __init__(self, *inputs, **kw):
        super(VerifyProjectCfg, self).__init__('.jobsets', *inputs, **kw)


class VerifyInput(object):
    def __init__(self, inpName, inpType, inpValue, contains=False):
        self.inpName = inpName
        self.inpType = inpType
        self.inpValue = inpValue
        self.contains = contains

    def verify(self, inp):
        print(self.__class__.__name__, 'verify', self.inpName)
        assert_eq(inp['type'], self.inpType)
        if self.contains:
            assert_in(self.inpValue, inp['value'])
        else:
            assert_eq(self.inpValue, inp['value'])
