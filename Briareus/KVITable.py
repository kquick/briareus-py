from functools import reduce
import string

def as_string(v):
    return v if isinstance(v, str) else str(v)


class KVITable(object):
    """Create a table indexed by key/value pairs.  Each key has a set of
       possible values, and the table is indexed by the key+value
       combination for each key.
    """

    def __init__(self, kv=list(), valuecol_name=None, kv_frozen=False,
                 default_factory=None,
                 keyval_factory=None,
    ):
        """Initialize KVI table.

            * kv = array of keys OR dict of keys:[values]

            * kv_frozen = True if new keys and values can be provided via the .add method.

            * valuecol_name is the name of the value column (if needed), and defaults to "Value"

            * default_factory allows the table to act similar to a collections.defaultdict.

            * keyval_factory is a function that takes a previously
              undefined key and returns the default value to be used
              for entries that did not previously have that key.

        """
        self._kv_frozen = kv_frozen
        self._kv = kv if isinstance(kv, dict) else { e:[] for e in kv }   # Presumes py3.7 stable ordering dict behavior
        self._entries = dict()  # dict_0(key = value for self._kv.keys()[0], value = dict_1(...)))
        self._valuecol_name = valuecol_name or 'Value'  # Name used for the value column (when needed)
        self._default_factory = default_factory
        self._keyval_factory = keyval_factory or (lambda key: '')

    def keyvals(self):
        return self._kv.copy()

    def add(self, entryval, *kv_tuples, **kv_spec):
        """Add value to (or overwrite) KVITable at position indexed by KV*,
        which is a specific value for each defined key.  If a key is
        missing from the input kv_tupes or kv_spec, an IndexError is
        raised.  A key or value can be added unless the KVITable was
        initialized with kv_frozen=True, in which case an IndexError
        will be raised.  If a new key is added, no value can be set as
        a leaf for the parent key (either before or after the new key
        is added) or an IndexError is raised. The *kv_tuples should be
        0 or more tuples, where each tuple specifies a KV pair
        (otherwise a ValueError is raised), and **kv_spec also
        specifies KV; the *kv_tuples and **kv_spec specifications are
        combined with indeterminate preference for duplications.  The
        use of *kv_tuples allows for situations where the K is not a
        valid keyword (e.g. it contains spaces).

        Note that as a special feature, entryval can be a callable
        which takes a single argument and returns a value.  The
        single argument is the current entry value at that location
        and the returned value is the new value to store.  This is
        useful when table items are being updated with new information
        (e.g. a count); the use of the default_factory argument is
        recommended in conjunction with this ability.

        """
        kseq = list(self._kv.keys())
        kvt = list(kv_tuples)
        for each in kvt:
            if not isinstance(each, tuple) or len(each) != 2:
                raise ValueError("kv arguments must each be a tuple of (key,value)")
        _, self._entries = self._addseq(entryval, self._entries, kseq, kvt + list(kv_spec.items()))

    def _addseq(self, entryval, tableentries, kseq, kvtuples):
        if (kseq or kvtuples) and not isinstance(tableentries, dict):
            raise IndexError('KVITable attempt to overwrite leaf value at %s of %s = %s'
                             % (str(kseq), str(list(self._kv.keys())), tableentries))
        if kseq:
            kvtdict = dict(kvtuples)
            key = kseq[0]
            rem_kseq = kseq[1:]
            if self._keyval_factory:
                val = kvtdict[key] if key in kvtdict else self._keyval_factory(key)
            else:
                val = kvtdict[key]  # raises KeyError if add call is missing a key
            if key in kvtdict:
                del kvtdict[key]
            if val not in self._kv[key]:
                if self._kv_frozen:
                    raise IndexError('KVITable is kv_frozen but got new value for key %s: %s' % (str(key), str(val)))
                self._kv[key].append(val)
            vsubtable = tableentries.get(val, dict())
            alk, tableentries[val] = self._addseq(entryval, vsubtable, rem_kseq, list(kvtdict.items()))
            if alk:
                tableentries = self._add_key_layer(val, alk, tableentries)
            return ([key] + alk if alk else []), tableentries
        if kvtuples:
            if self._kv_frozen:
                raise IndexError("KVITable is kv_frozen but add has extra: %s" % str(kvtuples))
            (key,val) = kvtuples[0]
            # Must update _kv (building downwards) before performing
            # bottom-up recursion of additional layers of kvtuples;
            # the exception handler removes this added key if the
            # recursive addition fails.
            self._kv[key] = [val]
            try:
                newkeylayer, tableentries[val] = self._addseq(entryval, dict(), [], kvtuples[1:])
            except Exception:
                del self._kv[key]
                raise
            if newkeylayer:
                tableentries = self._add_key_layer(val, newkeylayer, tableentries)
            return [key] + newkeylayer, tableentries
        # return the actual value; the tableentries passed in for this should have been an empty dict
        if isinstance(tableentries, dict) and len(tableentries) > 0:
            raise IndexError('KVITable attempt to set value when not at leaf: %s' % str(tableentries))
        if callable(entryval):
            if isinstance(tableentries, dict):
                # This was a default from the recursion
                return [], entryval(self._default_factory())
            return [], entryval(tableentries)
        return [], entryval

    def _add_key_layer(self, except_for_val, newkeys, tableentries):
        if newkeys:
            key = newkeys[0]
            remkeys = newkeys[1:]
            curvals = [v for v in tableentries.keys() if v != except_for_val]  # for stable iteration
            for val in curvals:
                if remkeys:
                    tableentries[val] = self._add_key_layer(None, remkeys, tableentries[val])
                else:
                    newval = self._keyval_factory(key)
                    tableentries[val] = dict([(newval, tableentries[val])])
                    if newval not in self._kv[key]:
                        self._kv[key].append(newval)
        return tableentries

    def get(self, *kv_tuples, **kv_spec):
        """Get table value at position indexed by KV*,
        which is a specific value for each defined key.  If a key or value is
        missing, an IndexError is raised.

        The *kv_tuples and **kv_spec arguments are used as described
        in the 'add' method.
        """
        kseq = list(self._kv.keys())
        kvt = list(kv_tuples)
        for each in kvt:
            if not isinstance(each, tuple) or len(each) != 2:
                raise ValueError("kv arguments must each be a tuple of (key,value)")
        return self._getseq(self._entries, kseq, kvt + list(kv_spec.items()))

    def _getseq(self, tableentries, kseq, kvtuples):
        if kseq:
            kvtdict = dict(kvtuples)
            key = kseq[0]
            rem_kseq = kseq[1:]
            val = kvtdict[key]  # raises KeyError if key is missing
            del kvtdict[key]
            if self._default_factory:
                if val not in tableentries:
                    return self._default_factory()
            vsubtable = tableentries[val]  # raises KeyError if key is missing
            return self._getseq(vsubtable, rem_kseq, list(kvtdict.items()))
        if kvtuples:
            raise IndexError("KVITable get with extra KV indexing: %s" % str(kvtuples))
        if isinstance(tableentries, dict) and len(tableentries) == 0:
            return self._default_factory()
        return tableentries

    def get_rows(self):
        """Returns the table as a raw list of rows with kv label columns and a
           final value column.
        """
        return self._get_rows(list(self._kv.keys()), self._entries)

    def _get_rows(self, kseq, tablecells):
        if kseq:
            key = kseq[0]
            rem_keys = kseq[1:]
            ret = []
            for each in sorted(self._kv[key]):
                if each in tablecells:  # skip rows with no entries
                    ret.extend([ [each] + l for l in self._get_rows(rem_keys, tablecells[each]) ])
            return ret
        return [ [tablecells] ]

    def get_entries_matching(self, **path):
        """Return every entry in the table that matches the (possibly partial)
           path (specified as a dictionary); any path component not
           mentioned is assumed to match all corresponding values that
           path component (i.e. all values for that key are returned).
           The return is a list of matching entries, where each
           element of the list is a tuple of '(
           [(key0,val0),(key1,val1),...], cell_entry )' where the
           first element of the tuple is the *full* path to the
           corresponding cell entry.

           Not very efficient (must essentially walk entire table).
        """
        return self._get_entries_matching(path, ([], self._kv), self._entries)

    def _get_entries_matching(self, path, path_and_kvs, path_tablecells, include_blanks=False):
        curpath, kv = path_and_kvs
        if not kv:
            return [(curpath, path_tablecells or "")]
        key = list(kv.keys())[0]
        vals = [path[key]] if key in path else kv[key]
        subkv = kv.copy()
        del subkv[key]
        return reduce(lambda l, m: l + m,
                      [ self._get_entries_matching(path,
                                                   (curpath + [(key,val)], subkv),
                                                   path_tablecells.get(val, dict()),
                                                   include_blanks=include_blanks,
                      )
                        for val in vals
                        if include_blanks or val in path_tablecells
                      ], [])

    def render(self, as_format='ascii',
               hide_blank_rows=True,
               hide_blank_cols=True,
               equisized_cols=True,
               sort_vals=False,
               colstack_at=None,
               row_repeat=True,
               row_group=None,
               valstr=None,
               entrystr=None,
               **kw):
        """Return the rendering of the table in the specified format (default=ascii).

            * hide_blank_rows is True (default) to remove rows for which there is no value(s)

            * hide_blank_cols is True (default) to remove columns for which there is no value(s)

            * equisized_cols is True (default) to maintain a
              consistent column width, otherwise columns are shrunk to
              the minimum size needed to display the title and values.
              This parameter is ignored for the HTML output format.

            * row_repeat is True (default) if an identical entry is to be repeated in subsequent rows

            * row_group is a list of key values to be grouped with separator lines

            * colstack_at is the column key where keys should be stacked columns with vals as each column header

            * valstr is a function to convert the Val (column headers
              or row titles) to a rendered string; the default is to
              simply convert the value to a string (if necessary).

            * entrystr is a function to convert the table entry to a
              renderable string; the default is to simply convert the
              value to a string (if necessary).

            * caption [HTML-format only] specifies the caption markup text.

        """
        kseq = list(self._kv.keys())
        return {
            'ascii' : KVITable__Render_ASCII,
            'html' : KVITable__Render_HTML,
        }[as_format](self,
                     hide_blank_rows=hide_blank_rows,
                     hide_blank_cols=hide_blank_cols,
                     equisized_cols=equisized_cols,
                     sort_vals=sort_vals,
                     colstack_at=colstack_at,
                     row_repeat=row_repeat,
                     row_group=row_group,
                     valstr=valstr,
                     entrystr=entrystr,
                     **kw
        ).render()


# ######################################################################

class KVITable__Render_(object):
    def __init__(self, kvitable,
                 hide_blank_rows=True,
                 hide_blank_cols=True,
                 equisized_cols=True,
                 sort_vals=False,
                 colstack_at=None,
                 row_repeat=True,
                 row_group=None):
        self._table = kvitable
        self._hide_blank_rows = hide_blank_rows
        self._hide_blank_cols = hide_blank_cols
        self._equisized_cols = equisized_cols
        self._colstack_at = colstack_at
        self._row_repeat = row_repeat
        self._row_group = row_group
        self._valsort = sort_with_nums if sort_vals else list

# If the key starts or ends with a digit, then this should do a rough
# numeric sort on the expectation that the digits represent a version
# or some other numeric value.  As an approximation of a numeric sort,
# sort by word size and then string value.  This will result in [ "1",
# "2", "10", "50", "400" ], but would fail with [ "v1.0", "v2.0",
# "v3.0", "v2.0.5", "v1.0.0.3" ], but it's a reasonably fast heuristic
# and probably better than a straight ascii sort.
def sort_with_nums(l):
    return sorted(l, key=lambda v: (len(v),v)
                  if v and isinstance(v, str) and
                  (v[0] in string.digits or v[-1] in string.digits)
                  else (0,v))

# ######################################################################

class KVITable__Render_ASCII(KVITable__Render_):
    def __init__(self, *args, valstr=None, entrystr=None, **kw):
        super(KVITable__Render_ASCII, self).__init__(*args, **kw)
        self._valstr = valstr or as_string
        self._entrystr = entrystr or (lambda _, v: as_string(v))

    def render(self):
        kseq = list(self._table._kv.keys())
        fmt, hdr = self._ascii_renderhdrs(kseq)
        body = self._ascii_renderseq(fmt, kseq, self._table._entries)
        return '\n'.join(hdr+body)

    def _ascii_renderhdrs(self, kseq):
        hrows = self._hdrstep(kseq)
        fmt = hrows[-1][0]
        return fmt, ([ fmt.render(hdrvals) + ((' <- ' + trailer) if trailer else '')
                       for fmt, hdrvals, trailer in hrows ] +
                     [ fmt.render([Separator()] * len(fmt)) ])

    def _hdrstep(self, kseq):
        # Returns [ (FmtLine, hdrvals, trailer) ] where each entry is a header line, top to bottom
        if kseq:
            key = kseq[0]
            if self._colstack_at == key:
                # Switch over to column stacking mode computation
                return self._hdrvalstep(kseq)
            # still a row-oriented key+val configuration
            keyw = lambda: max(len(self._valstr(key)),
                               max([0] +
                                   list(map(len, [self._valstr(v) for v in self._table._kv[key]]))))
            nexthdrs = self._hdrstep(kseq[1:])
            return [
                (fmt.add_col_left(keyw),
                 # first line shows hdrval for non-colstack'd columns, others are blank
                 ([self._valstr(key)] if i == 0 else [self._valstr('')]) + hdrvals,
                 trailer)
                for i, (fmt, hdrvals, trailer) in enumerate(nexthdrs)
            ]
        # colstack_at wasn't recognized, so devolve to a non-colstack table
        valwidth = lambda: [ max(len(self._valstr(self._table._valuecol_name)),
                                 max([0] +
                                     [len(self._entrystr(zip(self._table._kv.keys(), row[:-1]),
                                                         row[-1]))
                                      for row in self._table.get_rows()])) ]  # KWQ
        return [ (FmtLine(valwidth), [self._valstr(self._table._valuecol_name)], '') ]

    def _hdrvalstep(self, kseq, **excursion):
        if kseq:
            key = kseq[0]
            if len(kseq) == 1:
                # Reached a leaf
                titles = self._valsort(self._table._kv[key])
                cwidths = [self.cellwidths(self._entrystr, **excursion, **{key:val}) for val in titles]
                fmt = FmtLine(
                    lambda:
                    (lambda cws: [ max(0 if self._hide_blank_cols and sum(cws[i]) == 0 else len(self._valstr(val)),
                                       max([0] +
                                           ([0] if sum(cws[i]) == 0 and self._hide_blank_cols else cws[i])))
                                   for i,val in enumerate(titles) ])
                    (cwidths)
                )
                return [ (fmt, [ self._valstr(t) for t in titles], key) ]
            else:
                vals = self._valsort(self._table._kv[key])
                numvals = len(vals)
                if self._equisized_cols and not self._hide_blank_cols:
                    onesub = self._hdrvalstep(kseq[1:], **excursion)
                    subhdrs = [[(f.copy(),t,k) for f,t,k in onesub] for n in range(numvals) ]
                else:
                    subhdrs = []
                    for val in vals:
                        subhdrs.append(self._hdrvalstep(kseq[1:], **excursion, **{key:val}))
                    if self._equisized_cols:
                        # Now make all *non-empty* columns equisized
                        maxcolwidth = max([max([max(fmt) for fmt,_,_ in sh]) for sh in subhdrs])
                        for subhgrp in subhdrs:
                            for subhdr in subhgrp:
                                subhdr[0].resize_each(maxcolwidth)

                tophdrfmt = FmtLine(lambda: [ (lambda subwidths: max(len(self._valstr(vals[idx])), subwidths)
                                               if subwidths else 0)
                                              (max([s.width() for s,_,_ in subhdrs[idx]]))
                                              for idx in range(numvals)])

                # Ensure that the width of the last sub-column is
                # padded to be at least the width of the headers.
                for t,s in zip(tophdrfmt,subhdrs):
                    if t > s[-1][0].width():
                        s[-1][0].pad_last(t - s[-1][0].width())

                return [
                    (tophdrfmt,
                     [ CenterCap_ASCII(self._valstr)(val) for val in vals ],
                     key)
                ] + ([] if not subhdrs else
                     reduce(lambda rs,es: [(r[0].extended_with(e[0]), r[1]+e[1], r[2])
                                           for (r,e) in zip(rs,es)], subhdrs))

        else:
            # Normally caught at 'len(kseq) == 1' leaves above.
            # Coming here meant the colstack_at matched the current
            # key but then this function was entered without a kseq,
            # which should be impossible.
            raise RuntimeError('Called _hdrvalstep with empty kseq after matching colstack_at in kseq')

    def _ascii_renderseq(self, fmt, kseq, tablecells):
        rows = self._ascii_rows(kseq, tablecells, [])
        return [ fmt.render(row) for _,row in rows ]

    def _ascii_rows(self, kseq, tablecells, path):
        if kseq:
            key = kseq[0]
            if self._colstack_at == key:
                return [(False, self._ascii_multival_rows(kseq, tablecells, path))]
            rem_keys = kseq[1:]
            ret = []
            for each in self._valsort(self._table._kv[key]):
                if each not in tablecells and self._hide_blank_rows:
                    continue
                eachval = tablecells.get(each, dict())
                ret.extend( [ (s, [self._valstr(each
                                                if (self._row_repeat or n == 0) and not s
                                                else '')] + l)
                              for n,(s,l) in enumerate(self._ascii_rows(
                                      rem_keys,
                                      eachval,
                                      path + [(key, each)])) ])
                addgrpline = self._row_group is not None and key in self._row_group
                if addgrpline:
                    grpline = (True, [ Separator() ] * len(ret[0][1]))
                    if ret[-1][0]:
                        ret[-1] = grpline
                    else:
                        ret.append( grpline )
            return ret
        return [ (False, [' ' if tablecells == dict() else self._entrystr(path, tablecells)]) ]

    def _ascii_multival_rows(self, kseq, tablecells, pathstart):
        return [self._entrystr(pathstart + path,entry)
                for path,entry in self._table._get_entries_matching(
                        {},
                        ([], { k:self._valsort(self._table._kv[k]) for k in kseq }),
                        tablecells,
                        include_blanks=True,
                )]

    def cellwidths(self, entrystr, **path):
        """Get a list of the widths of every value in the table that matches
           the (possibly partial) path elements."""
        return [ len(entrystr(p,e)) for p,e in self._table.get_entries_matching(**path) ]


class Separator(object): pass

class CenterCap(object):
    def __init__(self, valstr=None, val=None):
        self._valstr = valstr or as_string
        self._val = val or ''
    def __call__(self, val):
        return self.__class__(self._valstr, val)
    def render(self, width, cap):
        raise NotImplementedError('subclass must define a render method')

class CenterCap_ASCII(CenterCap):
    def render(self, width, cap):
        l = len(self._valstr(self._val))
        clen,rlen = divmod(width - l - 2, 2)
        return ' '.join(filter(None, [(cap * clen) or None,
                                      self._valstr(self._val),
                                      (cap * (clen + rlen)) or None]))

class FmtLine(object):
    def __init__(self, col_func=None, sigils=None, sepline_sigils=None):
        self._cols = col_func() if col_func else list()
        self._sigils = sigils or { 'sep': '|', 'pad': ' ', 'cap': '_' }
        self._sepline_sigils = sepline_sigils or { 'sep': '+', 'pad': '-', 'cap': '_' }
        # n.b. the width of each sigil should be the same between
        # _sigils and _sepline_sigils for proper formatting.
    def __repr__(self):
        return '<FmtLine:' + repr(self._cols) + '>'
    def copy(self):
        r = FmtLine()
        r._cols = list(self._cols)
        r._sigils = dict(self._sigils)
        r._sepline_sigils = dict(self._sepline_sigils)
        return r
    def add_col_left(self, cwidth_fun):
        self._cols.insert(0, cwidth_fun())
        return self
    def repeat(self, count):
        self._cols = self._cols * count
        return self
    def width(self):
        s = sum(self)
        if s:
            return (s +
                    (len(self._sigils['pad']) * 2 +
                     len(self._sigils['sep'])) *
                    (len([c for c in self._cols if c]) - 1))
        return 0
    def __len__(self):
        return len(self._cols)
    def extended_with(self, other):
        self._cols = self._cols + other._cols
        return self
    def pad_last(self, padwidth):
        self._cols[-1] += padwidth
    def resize_each(self, tgtwidth, skip_zero=True):
        self._cols = [(tgtwidth if c else 0) for c in self._cols]
    def __getitem__(self, idx):
        return self._cols[idx]
    def render(self, flds):
        if len(flds) != len(self):
            raise ValueError('Insufficient arguments (%d) for FmtLine (%%d)' % len(flds) % len(self))
        return ''.join(
            # Left edge
            [ self._sepline_sigils['sep'] if isinstance(flds[0], Separator) else self._sigils['sep'] ]
            +
            [ '%%(pad)s%%%%%d.%ds%%(pad)s'
              % (self[i], self[i])
              % (self._sepline_sigils if isinstance(fld, Separator) else self._sigils)
              % ('-' * self[i] if isinstance(fld, Separator) else
                 (fld.render(self[i], self._sigils['cap'])
                  if isinstance(fld, CenterCap) else fld))
              + (self._sepline_sigils['sep']
                 if isinstance(fld, Separator) or
                 (i < len(flds) - 1 and isinstance(flds[i+1], Separator))
                 else self._sigils['sep'])
              for i,fld in enumerate(flds)
              if self[i]
            ])


# ######################################################################

class KVITable__Render_HTML(KVITable__Render_):
    def __init__(self, *args, caption=None, valstr=None, entrystr=None, **kw):
        super(KVITable__Render_HTML, self).__init__(*args, **kw)
        self._valstr = (lambda v: RenderVal(valstr(v))) if valstr else RenderVal
        self._entrystr = (lambda p,e: RenderCell(p, entrystr(p,e))) if entrystr else RenderCell
        self._caption = caption

    def render(self):
        kseq = list(self._table._kv.keys())
        fmt, hdr = self._html_renderhdrs(kseq)
        body = self._html_renderseq(fmt, kseq, self._table._entries)
        return '\n'.join(['<table class="kvitable">',
                          ('<caption>%s</caption>' % self._caption) if self._caption else '',
                          '<thead class="kvitable_head">',
                         '\n'.join(hdr),
                         '</thead><tbody class="kvitable_body">',
                         '\n'.join(body),
                         '</tbody></table>',
        ])


    def _html_renderhdrs(self, kseq):
        hrows, rowfmt = self._hdrstep(kseq)
        # n.b. len(flds) may be > len(self._colspans) if some of the
        # fields span multiple rows and this is not the first row; in
        # that case there was no add_col_left for the remaining rows,
        # but the value still exists in flds, so ignore it.
        return rowfmt, ([ fmt.render(hdrvals[-len(fmt):], trailer)
                          for fmt, hdrvals, trailer in hrows ]
        )

    def _hdrstep(self, kseq):
        # Returns [ (FmtLine, hdrvals, trailer) ] where each entry is a header line, top to bottom
        if kseq:
            key = kseq[0]
            if self._colstack_at == key:
                # Switch over to column stacking mode computation
                return self._hdrvalstep(kseq)
            # still a row-oriented key+val configuration
            nexthdrs, lowestfmt = self._hdrstep(kseq[1:])
            fstnxt_fmt, fstnxt_hdrvals, fstnxt_trailer = nexthdrs[0]
            return ([ (fstnxt_fmt.add_col_left(),
                       [self._valstr(key).set_height(len(nexthdrs))] + fstnxt_hdrvals,
                       fstnxt_trailer)
                    ] + nexthdrs[1:],
                    lowestfmt.add_col_left())
        # colstack_at wasn't recognized, so devolve to a non-colstack table
        return [ (FmtLine_HTML([1]), [self._valstr(self._table._valuecol_name)], '') ], FmtLine_HTML([1])

    def _hdrvalstep(self, kseq, **excursion):
        if kseq:
            key = kseq[0]
            if len(kseq) == 1:
                # Reached a leaf
                titles = self._valsort(self._table._kv[key])
                validt = titles if not self._hide_blank_cols else \
                    [ val
                      for val in titles
                      if not all([(entry is None)
                                  for _,entry in self._table.get_entries_matching(**excursion, **{key:val}) ]) ]
                fmt = FmtLine_HTML([v in validt for v in titles])
                return [ (fmt, [ self._valstr(t) for t in titles], key) ], fmt.copy()
            else:
                vals = self._valsort(self._table._kv[key])
                numvals = len(vals)
                if self._hide_blank_cols:
                    subhdrs = [ self._hdrvalstep(kseq[1:], **excursion, **{key:val})[0]
                                for val in vals ]
                else :
                    subhdrs = [ self._hdrvalstep(kseq[1:], **excursion)[0] ] * numvals
                val_colspan = len(subhdrs[-1][-1][0])
                subhdr_rollup = [ reduce(lambda a, b: (a[0].extend_right(b[0]), a[1]+b[1], a[2]), e) for e in zip(*tuple(subhdrs)) ]
                hdrvalid = [ v[0][0].numcols() for v in subhdrs ]
                tophdr = (FmtLine_HTML(existing_colspans=hdrvalid),
                           [ CenterCap_HTML(self._valstr)(val) for val in vals ],
                           key)
                return ([ tophdr ] + subhdr_rollup, subhdr_rollup[-1][0].copy())
        else:
            # Normally caught at 'len(kseq) == 1' leaves above.
            # Coming here meant the colstack_at matched the current
            # key but then this function was entered without a kseq,
            # which should be impossible.
            raise RuntimeError('Called _hdrvalstep with empty kseq after matching colstack_at in kseq')

    def _html_renderseq(self, fmt, kseq, tablecells):
        rows = self._html_rows(kseq, tablecells, [])
        return [ fmt.render(row) for _,row in rows ]

    def _html_rows(self, kseq, tablecells, path):
        if kseq:
            key = kseq[0]
            if self._colstack_at == key:
                return [(False, self._html_multival_rows(kseq, tablecells, path))]
            rem_keys = kseq[1:]
            ret = []
            for each in self._valsort(self._table._kv[key]):
                if each not in tablecells and self._hide_blank_rows:
                    continue
                eachval = tablecells.get(each, dict())
                rightrows = self._html_rows(rem_keys, eachval, path + [(key, each)])
                addgrpline = self._row_group is not None and key in self._row_group
                if self._row_repeat:
                    ret.extend( [ (s, [self._valstr(each).add_class('last_in_group' if s else None)] + l)
                                  for s,l in rightrows ] )
                else:
                    fst_lastgrp, fst_rightrows = rightrows[0]
                    ret.extend( [ (rightrows[-1][0],
                                   [self._valstr(each)
                                    .set_height(len(rightrows))
                                    .add_class('last_in_group' if (addgrpline or rightrows[-1][0]) else None)] + fst_rightrows)
                                  ] + rightrows[1:] )
                if addgrpline:
                    ret[-1] = (True, [ each.add_class('last_in_group') for each in ret[-1][1] ])
            return ret
        return [ (False, [' ' if tablecells == dict() else self._entrystr(path, tablecells)]) ]  # no need for first elem

    def _html_multival_rows(self, kseq, tablecells, pathstart):
        return [self._entrystr(pathstart + path,entry)
                for path,entry in self._table._get_entries_matching(
                        {},
                        ([], { k:self._valsort(self._table._kv[k]) for k in kseq }),
                        tablecells,
                        include_blanks=True,
                )]


class HTML__Elem(object):
    def __init__(self, val, tag=None, classes=None):
        self._val = val if isinstance(val, str) else str(val)
        self._tag = tag or 'span'
        self._classes = None if classes is None else set(classes)
    def __repr__(self):
        return self._tag + '::' + self._val
    def add_class(self, newclass):
        if newclass is not None:
            if self._classes is None:
                self._classes = set()
            self._classes.add(newclass)
        return self
    def other_tag_attrs(self): return []
    def render(self):
        return ''.join(['<',
                        ' '.join(filter(None, [ self._tag ] +
                                        [('class="' + ' '.join(self._classes) + '"')
                                         if self._classes else None] +
                                        self.other_tag_attrs())),
                        '>',
                        self._val,
                        '</',
                        self._tag,
                        '>'])

class HTML__TableElem(HTML__Elem):
    def __init__(self, *args, **kw):
        super(HTML__TableElem, self).__init__(*args, **kw)
        self._colspan = 1
        self._rowspan = 1
    def other_tag_attrs(self):
        return list(filter(None,
                           [ 'colspan=%d'%self._colspan if self._colspan > 1 else None,
                             'rowspan=%d'%self._rowspan if self._rowspan > 1 else None,
                           ]))
    def set_height(self, nrows):
        self._rowspan = nrows
        return self
    def render(self, width, unk):
        self._colspan = width
        return super(HTML__TableElem, self).render()

class CenterCap_HTML(CenterCap):
    def render(self, width, cap):
        return '<th class="kvitable_th multicol" colspan=%d>'%width + self._val + "</th>"

class RenderVal(HTML__TableElem):
    def __init__(self, val):
        super(RenderVal, self).__init__(val, tag='th', classes=["kvitable_th"])

class RenderCell(HTML__TableElem):
    def __init__(self, _path, entry):
        super(RenderCell, self).__init__(entry, tag='td', classes=["kvitable_td"])

class FmtLine_HTML(object):
    def __init__(self, enabled_columns=list(), colspan=1, existing_colspans=list()):
        if existing_colspans:
            if enabled_columns:
                raise ValueError("Should specify either enabled_columns or existing_colspans")
            self._colspans = list(existing_colspans)
        else:
            self._colspans = [ colspan if e else 0 for e in enabled_columns ]
    def copy(self):
        r = FmtLine_HTML()
        r._colspans = list(self._colspans)
        return r
    def __repr__(self):
        return '<FmtLine_HTML:' + repr(self._colspans) + '>'
    def __len__(self):
        return len(self._colspans)
    def numcols(self):
        return sum([c for c in self._colspans if c])
    def extend_right(self, other):
        r = self.copy()
        r._colspans.extend(other._colspans)
        return r
    def add_col_left(self):
        self._colspans.insert(0, 1)
        return self
    def render(self, flds, rightlabel=None):
        # n.b. len(flds) may be < len(self._colspans) if some of the
        # fields span multiple rows and this is not the first row; in
        # that case the fmt has the full set of colspans, but the vals
        # is only the rightmost
        return ''.join(
            [ '<tr class="kvitable_tr">' ]
            +
            [ (fld.render(colspan, '') if hasattr(fld, 'render') else
               ('<th class="kvitable_th">' + fld + '</th>'))
              for fld, colspan in zip(flds, self._colspans[-len(flds):])
              if colspan
            ]
            +
            ([RenderVal('&nbsp;&larr;' + rightlabel).add_class('rightlabel').render(1, '')]
             if rightlabel else [])
            +
            [ '</tr>' ]
        )
