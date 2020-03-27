"Generate an HTML summary of a build report"

from collections import defaultdict
from Briareus.KVITable import KVITable
from Briareus.Types import (StatusReport, PendingStatus, NewPending, Notify, PR_Status, PRCfg, BranchCfg)
from Briareus.BuildSys import buildcfg_name
from Briareus.AnaRep.TextSummary import tbl_branch, tbl_branch_
import datetime


_inc = lambda n: n + 1
_dec = lambda n: n - 1

class TCell_Bld(object):
    def __init__(self, project, bldname):
        self._project = project
        self._bldname = bldname
    def render(self, make_builder_url=None):
        if make_builder_url:
            url = make_builder_url(self._project, self._bldname)
            return ('<a href="%(url)s" class="bldsts %(ststype)s">%(cell)s</a>'
                    %
                    { 'url': url,
                      'ststype': self.ststype,
                      'cell': str(self),
                    })
        return str(self)
    def __call__(self, orig):
        # Callable for combining with previous entry at this location in the table
        if orig is None:
            return self
        if isinstance(orig, TCell_PendingBld):
            orig.set_prev(self)
            return orig
        return None


class TCell_GoodBld(TCell_Bld):
    ststype = "good"
    def __str__(self): return 'ok'

class TCell_BadCfgBld(TCell_Bld):
    ststype = "badcfg"
    def __str__(self): return '-CFG'

class TCell_FailBld(TCell_Bld):
    ststype = "fail"
    def __init__(self, project, bldname, failcnt):
        super(TCell_FailBld, self).__init__(project, bldname)
        self._failcnt = int(failcnt)
    def __call__(self, orig):
        # Callable for combining with previous entry at this location in the table
        r = super(TCell_FailBld, self).__call__(orig)
        if r is not None:
            return r
        if isinstance(orig, TCell_FailBld):
            self._failcnt += orig.cnt()
            return self
        if not isinstance(orig, TCell_FailBld):
            raise ValueError('Combine TCell_FailBld with old %s: unsupported' %
                             str(type(orig)))
    def cnt(self):
        return self._failcnt
    def __str__(self): return 'FAIL:%d' % self.cnt()

class TCell_PendingBld(TCell_Bld):
    ststype = "pending"
    def __init__(self, project, bldname):
        super(TCell_PendingBld, self).__init__(project, bldname)
        self._prev = None
    def set_prev(self, prev):
        self._prev = prev
    def __str__(self):
        if self._prev:
            return '(' + str(self._prev) + ')?'
        return '??'

def tcell_entshow(base_builder_url):
    def _t_es(path, ent):
        if isinstance(ent, TCell_Bld):
            if base_builder_url:
                mkurl = lambda p, n: "/".join([base_builder_url, "jobset", p, n])
                return ent.render(mkurl)
            return ent.render()
        return str(ent)
    return _t_es

def htbl_branch(sr, repdata):
    tblbranch = tbl_branch(sr)
    return htbl_branch_upd(tblbranch, sr.branchtype, sr.branch, sr.blddesc, repdata)

def htbl_branch_(bldname, branch, newpending, repdata):
    tblbranch = tbl_branch_(bldname, branch)
    return htbl_branch_upd(tblbranch,
                           newpending.bldcfg.branchtype,
                           newpending.bldcfg.branchname,
                           newpending.bldcfg.description,
                           repdata)

def htbl_branch_upd(tblbranch, branchtype, branch, blddesc, repdata):
    if branchtype == 'pullreq':
        for sr2 in repdata:
            if isinstance(sr2, PR_Status) and \
               sr2.branch == branch and \
               sr2.prtype == blddesc:
                bref = ', '.join([
                    '&nbsp;'.join( [c.reponame, 'PR', '#' + c.pr_id]  # PRCfg
                                   if isinstance(c, PRCfg) else
                                   [c.reponame, 'branch'] ) # BranchCfg
                    for c in sr2.prcfg
                ])
                tblbranch = ''.join([
                    '<div class="tooltip">',
                    tblbranch,
                    '<span class="tooltiptext">',
                    bref,
                    '</span>',
                    '</div>',
                ])
                break;
    return tblbranch


def html_summary(repdata, base_builder_url=None):
    entshow_fun = tcell_entshow(base_builder_url)

    projects = set([ sr.project for sr in repdata if isinstance(sr, StatusReport) ])

    summary = KVITable(default_factory=int, valuecol_name='Total')
    summary.add(len(projects), Element='Projects')
    summary.add(len(set([sr.branch for sr in repdata
                         if isinstance(sr, StatusReport) and sr.branchtype=='pullreq'])), Element='Pull Requests')

    projtable = KVITable({
        'Project': sorted(list(set([ sr.project
                                     for sr in repdata
                                     if isinstance(sr, StatusReport) ]))),
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
                         default_factory=lambda: None,
                         keyval_factory=lambda key: 'x86_64-linux' if key == 'system' else 'n/a',
                         kv_frozen=False)

    mkDetailTable = lambda: KVITable({
        'system': ['x86-64_linux'],
        'Branch': [],
        'Strategy': ['regular', 'submodules', 'HEADs'],
    },
                                     valuecol_name='Build Status',
                                     default_factory=lambda: None,
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
            tblbrname = htbl_branch(sr, repdata)

            fulltable.add(TCell_PendingBld(sr.project, sr.buildname),
                          *vars,
                          Project=sr.project,
                          Branch=tblbrname,
                          Strategy=sr.strategy)
            detailtables[sr.project].add(TCell_PendingBld(sr.project, sr.buildname),
                                         *vars,
                                         Branch=tblbrname,
                                         Strategy=sr.strategy)

        elif isinstance(sr, NewPending):
            summary.add(_inc, Element='Builds')
            projectname = sr.bldcfg.projectname
            buildname = buildcfg_name(sr.bldcfg)
            tbl_brname = htbl_branch_(buildname, sr.bldcfg.branchname, sr, repdata)
            projtable.add(_inc, Project=projectname, Status="TOTAL")
            projtable.add(_inc, Project=projectname, Status="pending")
            vars = tuple([ (v.varname, v.varvalue) for v in sr.bldcfg.bldvars ])

            fulltable.add(TCell_PendingBld(projectname, buildname),
                          *vars,
                          Project=projectname,
                          Branch=tbl_brname,
                          Strategy=sr.bldcfg.strategy)
            detailtables[sr.bldcfg.projectname].add(
                TCell_PendingBld(projectname, buildname),
                *vars,
                Branch=tbl_brname,
                Strategy=sr.bldcfg.strategy)

        elif isinstance(sr, StatusReport):
            summary.add(_inc, Element='Builds')

            projtable.add(_inc, Project=sr.project, Status=projtable_sts(sr.status))
            projtable.add(_inc, Project=sr.project, Status='TOTAL')

            bldres = { 'initial_success' : TCell_GoodBld,
                       'succeeded' : TCell_GoodBld,
                       'fixed' : TCell_GoodBld,
                       'bad_config' : TCell_BadCfgBld,
            }.get(sr.status,
                  lambda proj, name: TCell_FailBld(proj, name, sr.status)
            )(sr.project, sr.buildname)

            tblbrname = htbl_branch(sr, repdata)

            fulltable.add(bldres,
                          *tuple([ (v.varname, v.varvalue) for v in sr.bldvars ]),
                          Project=sr.project,
                          Branch=tblbrname,
                          Strategy=sr.strategy)

            detailtables[sr.project].add(bldres,
                                         *tuple([ (v.varname, v.varvalue) for v in sr.bldvars ]),
                                         Branch=tblbrname,
                                         Strategy=sr.strategy)

    section_hdrfun = lambda msg, idref: '<h2 class="section_hdr" id="' + idref + '">' + msg + '</h2><div class="tdata">'
    subsection_hdrfun = section_hdrfun
    section_endfun = lambda: '</div>'

    nav = '\n'.join([
        '<ul class="nav">',
        '<li><a href="#stats">Top</a></li>',
        '<li><a href="#project_summary">Summary</a></li>',
        '<li><a href="#combined">Combined</a></li>',
    ] + [ '<li><a href="#' + p + '">' + p + '</a></li>'
          for p in sorted(projects)
    ] + [
        '<li id=\"navdate\">Updated: ' + str(datetime.datetime.now()) + '</li>',
        '</ul>',
    ])

    if False:
        summary_render = summary.render(as_format='html', sort_vals=True)
    else:
        summary_render = '\n'.join([
            '<div class="row">',
        ] + [ '\n'.join([ '<div class="column">',
                          '<div class="card">',
                          '<p><i class="fa ' + cls + '"></i></p>',
                          '<h3>' + str(summary.get(('Element',element))) + '</h3>',
                          '<p>' + element + '</p>',
                          '</div>',
                          '</div>',
                        ])
              for (element,cls) in [ ('Projects','fa-briefcase'),
                                     ('Pull Requests','fas fa-edit'),
                                     ('Builds', 'fa-wrench'),
                                     ('Notifications', 'fas fa-paper-plane'),
              ]
        ] + [
            '</div>',
        ])

    return '\n\n'.join([
        nav,
        section_hdrfun('Statistics', 'stats'),
        summary_render,
        section_hdrfun('Per-project Build Status Summary', 'project_summary'),
        projtable.render(row_group=['Project'],
                         row_repeat=False,
                         sort_vals=False,
                         as_format='html',
                         caption='Per-project Build Status Summary',
                         colstack_at='Status'),
        section_endfun(),
        section_hdrfun('Combined Details', 'combined'),
        fulltable.render(row_group=['system', 'Branch', 'Strategy'],
                         row_repeat=False,
                         sort_vals=True,
                         entrystr=entshow_fun,
                         as_format='html',
                         caption='Combined Details',
                         colstack_at=(list(fulltable.keyvals().keys()) + [None])[4],),
        section_endfun(),
        # section_hdrfun('Individual Project Summaries ::'),
        ] + [ '\n\n'.join([ subsection_hdrfun('Project %s' % p, p),
                            detailtables[p].render(row_repeat=False,
                                                   as_format='html',
                                                   caption='Project %s' % p,
                                                   sort_vals=True,
                                                   colstack_at=(list(detailtables[p].keyvals().keys()) + [None])[3],
                                                   row_group=['system', 'Branch'],
                                                   entrystr=entshow_fun,
                            ),
                            section_endfun(),
                           ])
            for p in sorted(projects)
        ])
