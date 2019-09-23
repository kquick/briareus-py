import pprint
from Briareus.Types import *


def get_prior_report(report_fname):
    with open(report_fname, 'r') as repf:
        rep = repf.read()
    if rep:
        try:
            return [ eval(l.replace('StatusReport', 'PriorStatus'),
                          globals(), {})
                     for l in rep.split('\n\n')
                     if l.strip() ]
        except Exception as e:
            print('Warning: unable to process prior report data:', str(e))
    return []

def write_report_output(reportf, report):
    for each in report:
        pprint.pprint(each, stream=reportf)
        print('', file=reportf)
