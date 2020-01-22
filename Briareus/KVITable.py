from functools import reduce

class Separator(object): pass
class CenterCap(object):
    def __init__(self, val):
        self._val = val
    def render(self, width, cap):
        l = len(self._val)
        clen,rlen = divmod(width - l - 2, 2)
        return ' '.join([cap * clen, self._val, cap * (clen + rlen)])

class FmtLine(list):
    def __init__(self, *args, sigils=None, sepline_sigils=None):
        super(FmtLine, self).__init__(*args)
        self._sigils = sigils or { 'sep': '|', 'pad': ' ', 'cap': '_' }
        self._sepline_sigils = sepline_sigils or { 'sep': '+', 'pad': '-', 'cap': '_' }
        # n.b. the width of each sigil should be the same between
        # _sigils and _sepline_sigils for proper formatting.
    def add_col_left(self, cwidth):
        self.insert(0, cwidth)
        return self
    def repeat(self, count):
        return FmtLine(self * count, sigils=self._sigils, sepline_sigils=self._sepline_sigils)
    def width(self):
        return sum(self) + (len(self._sigils['pad']) * 2 + len(self._sigils['sep'])) * (len(self) - 1)
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
            ])

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

    def render(self, format='ascii', hide_blank_rows=True,
               colstack_at=None,
               row_repeat=True,
               row_group=None,
               valstr=as_string,
               entrystr=as_string):
        """Return the rendering of the table in the specified format (default=ascii).

            * hide_blank_rows is True (default) to remove rows for which there is no value(s)

            * row_repeat is True (default) if an identical entry is to be repeated in subsequent rows

            * row_group is a list of key values to be grouped with separator lines

            * colstack_at is the column key where keys should be stacked columns with vals as each column header

            * valstr is a function to convert the Val (column headers
              or row titles) to a rendered string; the default is to
              simply convert the value to a string (if necessary).

            * entrystr is a function to convert the table entry to a
              renderable string; the default is to simply convert the
              value to a string (if necessary).

        """
        kseq = list(self._kv.keys())
        kvwidths = [ max(len(valstr(key)), max(map(len, [valstr(v)
                                                         for v in self._kv[key]])))
                     for key in kseq ]
        return { 'ascii' : self._render_ascii }[format](kseq,
                                                        kvwidths,
                                                        hide_blank_rows=hide_blank_rows,
                                                        colstack_at=colstack_at,
                                                        row_repeat=row_repeat,
                                                        row_group=row_group,
                                                        valstr=valstr,
                                                        entrystr=entrystr,
        )

    def _render_ascii(self, kseq, kvwidths, hide_blank_rows, colstack_at, row_repeat, row_group, valstr, entrystr):
        fmt, hdr = self._ascii_renderhdrs(kseq,
                                          kvwidths,
                                          valstr=valstr,
                                          entrystr=entrystr,
                                          colstack_at=colstack_at,
        )
        body = self._ascii_renderseq(fmt, kseq, self._entries,
                                     hide_blank_rows=hide_blank_rows,
                                     colstack_at=colstack_at,
                                     row_repeat=row_repeat,
                                     row_group=row_group,
                                     valstr=valstr,
                                     entrystr=entrystr,
        )
        return '\n'.join(hdr+body)

    def _ascii_renderhdrs(self, kseq, kvwidths, valstr, entrystr, colstack_at=None):
        hrows = self._hdrstep(kseq, kvwidths, colstack_at, valstr=valstr, entrystr=entrystr)
        fmt = hrows[-1][0]
        return fmt, ([ fmt.render(hdrvals) + trailer
                       for fmt, hdrvals, trailer in hrows ] +
                     [ fmt.render([Separator()] * len(fmt)) ])


    def _hdrstep(self, kseq, kvwidths, colstack_at, valstr, entrystr):
        # Returns [ (FmtLine, hdrvals, trailer) ] where each entry is a header line, top to bottom
        if kseq:
            key = kseq[0]
            if colstack_at == key:
                # Switch over to column stacking mode computation
                return self._hdrvalstep(kseq, valstr, entrystr)
            # still a row-oriented key+val configuration
            keyw = kvwidths[0]
            nexthdrs = self._hdrstep(kseq[1:], kvwidths[1:], colstack_at, valstr, entrystr)
            return [
                (fmt.add_col_left(keyw),
                 # first line shows hdrval for non-colstack'd columns, others are blank
                 ([valstr(key)] if i == 0 else ['']) + hdrvals,
                 trailer)
                for i, (fmt, hdrvals, trailer) in enumerate(nexthdrs)
            ]
        # colstack_at wasn't recognized, so devolve to a non-colstack table
        valwidth = max(len(valstr(self._valuecol_name)),
                       max([len(entrystr(row[-1])) for row in self.get_rows()]))
        return [ (FmtLine([valwidth]), [valstr(self._valuecol_name)], '') ]

    def _hdrvalstep(self, kseq, valstr, entrystr):
        if kseq:
            key = kseq[0]
            if len(kseq) == 1:
                # Reached a leaf
                titles = self._kv[key]
                fmt = FmtLine([ max(len(valstr(val)),
                                    max([0] + self.cellwidths(entrystr, **{key: val})))
                                for val in titles ])
                return [ (fmt, [ valstr(t) for t in titles], ' <- ' + key) ]
            else:
                subhdrs = self._hdrvalstep(kseq[1:], valstr, entrystr)
                subhdrs_width = sum([subfmt.width() for subfmt,_,_ in subhdrs])
                vals = self._kv[key]
                numvals = len(vals)
                return [
                    (FmtLine([ max(len(valstr(val)), subhdrs_width) for val in vals]),
                     [ CenterCap(valstr(val)) for val in vals ],
                     ' <- ' + key)
                ] + [ (fmt.repeat(numvals), titles * numvals, trailer)
                      for fmt, titles, trailer in subhdrs ]
        else:
            # Normally caught at 'len(kseq) == 1' leaves above.
            # Coming here meant the colstack_at matched the current
            # key but then this function was entered without a kseq,
            # which should be impossible.
            raise RuntimeError('Called _hdrvalstep with empty kseq after matching colstack_at in kseq')

    def _ascii_renderseq(self, fmt, kseq, tablecells, hide_blank_rows, colstack_at, row_repeat, row_group,
                         valstr, entrystr):
        rows = self._ascii_rows(kseq, tablecells,
                                hide_blank_rows=hide_blank_rows,
                                row_repeat=row_repeat,
                                row_group=row_group,
                                colstack_at=colstack_at,
                                valstr=valstr,
                                entrystr=entrystr,
        )
        return [ fmt.render(row) for _,row in rows ]

    def _ascii_rows(self, kseq, tablecells, hide_blank_rows, row_repeat, row_group, colstack_at,
                    valstr, entrystr):
        if kseq:
            key = kseq[0]
            if colstack_at == key:
                return [(False, self._ascii_multival_rows(kseq, tablecells,
                                                          hide_blank_rows=hide_blank_rows,
                                                          row_repeat=row_repeat,
                                                          row_group=row_group,
                                                          valstr=valstr,
                                                          entrystr=entrystr,
                ))]
            rem_keys = kseq[1:]
            ret = []
            for each in self._kv[key]:
                if each not in tablecells and hide_blank_rows:
                    continue
                eachval = tablecells.get(each, dict())
                ret.extend( [ (s, [valstr(each) if (row_repeat or n == 0) and not s else ''] + l)
                              for n,(s,l) in enumerate(self._ascii_rows(rem_keys, eachval,
                                                                        hide_blank_rows=hide_blank_rows,
                                                                        row_repeat=row_repeat,
                                                                        row_group=row_group,
                                                                        colstack_at=colstack_at,
                                                                        valstr=valstr,
                                                                        entrystr=entrystr,
                              )) ])
                addgrpline = row_group is not None and key in row_group
                if addgrpline:
                    grpline = (True, [ Separator() ] * len(ret[0][1]))
                    if ret[-1][0]:
                        ret[-1] = grpline
                    else:
                        ret.append( grpline )
            return ret
        return [ (False, [' ' if tablecells == dict() else entrystr(tablecells)]) ]

    def _ascii_multival_rows(self, kseq, tablecells, hide_blank_rows, row_repeat, row_group,
                             valstr, entrystr):
        return [entrystr(entry)
                for _path,entry in self._get_entries_matching({},
                                                              ([], { k:self._kv[k] for k in kseq }),
                                                              tablecells,
                                                              include_blanks=True,
                )]

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
            for each in self._kv[key]:
                if each in tablecells:  # skip rows with no entries
                    ret.extend([ [each] + l for l in self._get_rows(rem_keys, tablecells[each]) ])
            return ret
        return [ [tablecells] ]

    def cellwidths(self, entrystr, **path):
        """Get a list of the widths of every value in the table that matches
           the (possibly partial) path elements."""
        return [ len(entrystr(e)) for p,e in self.get_entries_matching(**path) ]

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
