"Generate a Text summary of a build report"

from collections import defaultdict
from Briareus.KVITable import KVITable
from Briareus.Types import (StatusReport, PendingStatus, Notify)

_inc = lambda n: n + 1
_dec = lambda n: n - 1

class FailCount(int): pass

def _add_if_int(v):
    try:
        n = int(v)
        return lambda o: (o.cnt(n)
                          if isinstance(o, PendingBld)
                          else FailCount(o + n))
    except ValueError:
        return lambda o: o.sts(v) if isinstance(o, PendingBld) else v

class PendingBld(object):
    def __init__(self, failcnt):
        self._failcnt = failcnt
        self._prevsts = None
    def cnt(self, numfails):
        self._failcnt += numfails
        return self
    def sts(self, oldsts):
        self._prevsts = oldsts
        return self
    def __str__(self):
        if self._prevsts and self._failcnt:
            return ('TBD/error: %s and %d' % (self._prevsts, self._failcnt))
            # raise NotImplementedError('TBD/error: %s and %d' % (self._prevsts, self._failcnt))
        if self._prevsts:
            return '(' + self._prevsts + ')?'
        if self._failcnt:
            return '(-%d)?' % self._failcnt
        return '??'

def _show_with_fail(_path, v):
    if isinstance(v, FailCount):
        return 'FAIL*%d' % v
    if isinstance(v, PendingBld):
        return str(v)
    return v

def tbl_branch(r):
    # Don't necessarily want to just use the branch name
    # because it might be the main branch with multiple PR's
    # on that branch.  At this point, use the regularity of
    # the buildname to know that the start of the buildname,
    # up to a match with the branch name is the unique
    # identifier for this branch + PR.  However, the branch
    # name might have been adjusted to be a valid task name,
    # in which case the assumption is that the portion of the
    # buildname up to the first period is the desired Branch
    # table entry.  KWQ KWQ: needs a better approach!
    buildname_fp = r.buildname.split(r.branch)[0]
    return (r.buildname.split('.')[0]
            if buildname_fp == r.buildname
            else buildname_fp + r.branch)


def text_summary(repdata):
    sepline='='*60
    hashline='#'*60
    banner = '\n\n%(sepline)s\n%(hashline)s\n%(sepline)s\n\n'%locals()
    section_hdrfun = lambda msg: banner + msg
    subsection_hdrfun = lambda msg: msg + '\n'
    entshow_fun = _show_with_fail

    projects = set([ sr.project for sr in repdata if isinstance(sr, StatusReport) ])

    summary = KVITable(default_factory=int, valuecol_name='Total')
    summary.add(len(projects), Element='Projects')
    summary.add(len(set([sr.branch for sr in repdata
                         if isinstance(sr, StatusReport)
                         and sr.branchtype=='pullreq'])),
                Element='Pull Requests')

    projtable = KVITable({
        'Project': [],
        'Status': [ 'TOTAL', 'ok', 'FAIL', 'pending' ],
    },
                         valuecol_name='Number',
                         kv_frozen=False,
                         default_factory=int)

    fulltable = KVITable({
        'Branch': [],
        'system': ['x86-64_linux'],
        'Strategy': ['regular', 'submodules', 'HEADs'],
        'Project': [],
    },
                         valuecol_name='Build Status',
                         default_factory=FailCount,
                         keyval_factory=lambda key: 'x86_64-linux' if key == 'system' else 'n/a',
                         kv_frozen=False)

    mkDetailTable = lambda: KVITable({
        'system': ['x86-64_linux'],
        'Branch': [],
        'Strategy': ['regular', 'submodules', 'HEADs'],
    },
                                     valuecol_name='Build Status',
                                     default_factory=FailCount,
                                     keyval_factory=lambda key: 'x86_64-linux' if key == 'system' else '',
                                     kv_frozen=False)
    detailtables = defaultdict(mkDetailTable)
    projtable_sts = lambda s: { 'initial_success': 'ok',
                                'succeeded': 'ok',
                                # 'pending': 'pending',
    }.get(s, 'FAIL')

    for sr in repdata:

        if isinstance(sr, Notify):
            summary.add(_inc, Element='Notifications')

        elif isinstance(sr, PendingStatus):
            prev = [ r for r in repdata
                     if isinstance(r, StatusReport)
                     and r.project == sr.project
                     and r.buildname == sr.buildname ]
            if not prev:
                summary.add(_inc, Element='Builds')
                projtable.add(_inc, Project=sr.project, Status="TOTAL")
            else:
                projtable.add(_dec, Project=sr.project, Status=projtable_sts(prev[0].status))
            projtable.add(_inc, Project=sr.project, Status="pending")
            vars = tuple([ (v.varname, v.varvalue) for v in sr.bldvars ])

            fulltable.add(PendingBld,
                          *vars,
                          Project=sr.project,
                          Branch=tbl_branch(sr),
                          Strategy=sr.strategy)
            detailtables[sr.project].add(PendingBld,
                                         *vars,
                                         Branch=tbl_branch(sr),
                                         Strategy=sr.strategy)

        elif isinstance(sr, StatusReport):
            summary.add(_inc, Element='Builds')

            projtable.add(_inc, Project=sr.project, Status=projtable_sts(sr.status))
            projtable.add(_inc, Project=sr.project, Status='TOTAL')

            bldres = _add_if_int({ 'initial_success': '+',
                                   'succeeded': '+',
                                   'fixed': '+',
                                   # 'pending': '?',
                                   'bad_config': '-CFG',
            }.get(sr.status, sr.status))
            fulltable.add(bldres, *tuple([ (v.varname, v.varvalue) for v in sr.bldvars ]),
                          Project=sr.project,
                          Branch=tbl_branch(sr),
                          Strategy=sr.strategy)

            detailtables[sr.project].add(bldres,
                                         *tuple([ (v.varname, v.varvalue) for v in sr.bldvars ]),
                                         Branch=tbl_branch(sr),
                                         Strategy=sr.strategy)

    keytable = KVITable({'Symbol': []}, valuecol_name='Meaning', kv_frozen=False)
    keytable.add('Success', Symbol='+')
    keytable.add("'n' build components failed", Symbol='FAIL*n')
    keytable.add('Build configuration error', Symbol='-CFG')
    keytable.add('Pending, no previous builds', Symbol='??')
    keytable.add('Pending, previously suceeding', Symbol='(+)?')
    keytable.add('Pending, previous config error', Symbol='(-CFG)?')
    keytable.add("Pending, previously 'n' components failed", Symbol='(-n)?')

    return '\n\n'.join([
        summary.render(as_format='ascii',
        ),
        section_hdrfun('Per-project Build Status Summary ::'),
        projtable.render(row_group=['Project'],
                         row_repeat=False,
                         as_format='ascii',
                         colstack_at='Status'),
        section_hdrfun('Combined Details ::'),
        fulltable.render(row_group=['system', 'Branch', 'Strategy'],
                         row_repeat=False,
                         entrystr=entshow_fun,
                         as_format='ascii',
                         colstack_at=(list(fulltable.keyvals().keys()) + [None])[4],),
        section_hdrfun('Individual Project Summaries ::'),
        '\n\n'.join([subsection_hdrfun('Project %s:\n' % p) +
                     detailtables[p].render(row_repeat=False,
                                            as_format='ascii',
                                            colstack_at=(list(detailtables[p].keyvals().keys()) + [None])[3],
                                            row_group=['system', 'Branch'],
                                            entrystr=entshow_fun,
                     )
                     for p in projects]),
        section_hdrfun('KEY ::'),
        keytable.render(as_format='ascii'),
        ])
