from Briareus.KVITable import KVITable
import pytest

def test_empty_kvitable_create():
    kvit = KVITable()

def test_empty_kvitable_get():
    kvit = KVITable()
    with pytest.raises(IndexError) as idxerr:
        foo = kvit.get(('foo', 'bar'), moo="cow")
    assert 'foo' in str(idxerr.value)
    assert 'bar' in str(idxerr.value)
    assert 'moo' in str(idxerr.value)
    assert 'cow' in str(idxerr.value)

def test_empty_kvitable_add():
    kvit = KVITable()
    kvit.add("hi", ('moo', "cow"), foo='bar')
    hello = kvit.get(('foo', 'bar'), moo="cow")
    assert hello == "hi"

def test_empty_frozen_kvitable_add_refused():
    kvit = KVITable(kv_frozen=True)
    with pytest.raises(IndexError) as idxerr:
        kvit.add("hi", ('moo', "cow"), foo='bar')
    assert "frozen" in str(idxerr.value)

def test_non_leaf_kvitable_add_uses_default_key_val():
    kvit = KVITable()
    kvit.add("hi", ('moo', "cow"), foo='bar')
    kvit.add("oops", ('moo', "dog"))
    rows = kvit.get_rows()
    assert [
        ['cow', 'bar', 'hi'],
        ['dog', '', 'oops'],
    ] == rows

def test_frozen_kvitable_add_key_key_refused():
    kvit = KVITable({'foo':['bar','baz']}, kv_frozen=True)
    kvit.add("hi", foo='bar')
    with pytest.raises(IndexError) as idxerr:
        kvit.add("oops", ('dog', "bark"), foo='baz')
    assert 'kv_frozen' in str(idxerr.value)

def test_non_frozen_kvitable_add_key():
    kvit = KVITable({'foo':['bar','baz']}, kv_frozen=False)
    kvit.add("hi", foo='bar')
    kvit.add("yo", foo='baz', dog='woof')
    rows = kvit.get_rows()
    assert [
        [ 'bar', '', 'hi' ],
        [ 'baz', 'woof', 'yo'],
    ] == rows
    show = kvit.render()
    assert '\n'.join([
        '| foo |  dog | Value |',
        '+-----+------+-------+',
        '| bar |      |    hi |',
        '| baz | woof |    yo |',
    ]) == show

def test_non_frozen_kvitable_add_deep_key():
    kvit = KVITable({'foo':['bar','baz'],
                     'moon':['beam', 'pie'],
    },
                    kv_frozen=False,
                    valuecol_name='says',
                    keyval_factory=lambda key: '?')
    kvit.add("hi", foo='bar', moon='pie')
    kvit.add("yo", foo='baz', moon='beam', dog='woof')
    kvit.add("Excellent!", foo='Bill', moon='Ted', dog='arf arf')
    rows = kvit.get_rows()
    assert [
        [ 'Bill', 'Ted', 'arf arf', 'Excellent!'],
        [ 'bar', 'pie', '?', 'hi' ],
        [ 'baz', 'beam', 'woof', 'yo'],
    ] == rows
    show = kvit.render()
    assert '\n'.join([
        '|  foo | moon |     dog |       says |',
        '+------+------+---------+------------+',
        '| Bill |  Ted | arf arf | Excellent! |',
        '|  bar |  pie |       ? |         hi |',
        '|  baz | beam |    woof |         yo |',
    ]) == show

def test_kvitable_cannot_extend_leaf():
    kvit = KVITable()
    kvit.add("hi", ('moo', "cow"), foo='bar')
    with pytest.raises(IndexError) as idxerr:
        kvit.add("woof", ('moo', "cow"), foo='bar', dog='bark')
    assert 'overwrite leaf' in str(idxerr.value)

def test_kvitable_initial_kv_list():
    kvit = KVITable(['moo', 'foo'])
    kvit.add("hi", ('moo', "cow"), foo='bar')
    hello = kvit.get(('foo', 'bar'), moo="cow")
    assert hello == "hi"

def test_frozen_kvitable_initial_kv_list_refuses_new_values():
    kvit = KVITable(['moo', 'foo'], kv_frozen=True)
    with pytest.raises(IndexError) as idxerr:
        kvit.add("hi", ('moo', "cow"), foo='bar')
    assert 'frozen' in str(idxerr.value)

def test_kvitable_initial_kv_dict():
    kvit = KVITable({'moo':['cow'], 'foo':['bar', 'baz']})
    kvit.add("hi", ('moo', "cow"), foo='bar')
    kvit.add("howdy", ('moo', "cow"), foo='baz')
    hello = kvit.get(('foo', 'bar'), moo="cow")
    assert hello == "hi"
    hello2 = kvit.get(moo="cow", foo='baz')
    assert hello2 == "howdy"

@pytest.fixture()
def medium_kvitable():
    kvit = KVITable({'compiler': [ 'gcc7', 'gcc8', 'clang6', 'clang7' ],
                     'debug': ['yes', 'no'],
                     'optimization': [0, 1, 3],
                     },
                    kv_frozen=True)
    kvit.add('good', compiler='gcc7', debug='yes', optimization=0)
    kvit.add('bad', compiler='gcc7', debug='no', optimization=0)
    kvit.add('ugly', compiler='gcc7', debug='yes', optimization=3)
    kvit.add('good', compiler='gcc8', debug='yes', optimization=0)
    kvit.add('ok', compiler='clang6', debug='yes', optimization=0)
    kvit.add('good', compiler='clang7', debug='no', optimization=1)
    kvit.add('good', compiler='clang7', debug='no', optimization=3)
    kvit.add('good', compiler='clang7', debug='yes', optimization=3)
    kvit.add(True, compiler='gcc8', debug='yes', optimization=3)
    kvit.add('bad', compiler='gcc8', debug='yes', optimization=1)
    kvit.add('good', compiler='clang7', debug='no', optimization=0)
    kvit.add('good', compiler='gcc7', debug='no', optimization=1)
    return kvit

def test_medium_kvitable_get_rows(medium_kvitable):
    rows = medium_kvitable.get_rows()
    assert [
        [ 'clang6', 'yes', 0, 'ok' ],
        [ 'clang7', 'no', 0, 'good' ],
        [ 'clang7', 'no', 1, 'good' ],
        [ 'clang7', 'no', 3, 'good' ],
        [ 'clang7', 'yes', 3, 'good' ],
        [ 'gcc7', 'no', 0, 'bad' ],
        [ 'gcc7', 'no', 1, 'good' ],
        [ 'gcc7', 'yes', 0, 'good' ],
        [ 'gcc7', 'yes', 3, 'ugly' ],
        [ 'gcc8', 'yes', 0, 'good' ],
        [ 'gcc8', 'yes', 1, 'bad' ],
        [ 'gcc8', 'yes', 3, True ],
    ] == rows

def test_medium_kvitable_get_path_beg(medium_kvitable):
    r = medium_kvitable.get_entries_matching(compiler='gcc7')
    assert [ ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 0)], 'good'),
             ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 3)], 'ugly'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 0)], 'bad'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)], 'good'),
    ] == r

def test_medium_kvitable_get_path_mid(medium_kvitable):
    r = medium_kvitable.get_entries_matching(debug='no')
    assert [ ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 0)], 'bad'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 0)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 3)], 'good'),
    ] == r

def test_medium_kvitable_get_path_end(medium_kvitable):
    r = medium_kvitable.get_entries_matching(optimization=1)
    assert [ ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 1)], 'bad'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)], 'good'),
    ] == r

def test_medium_kvitable_get_path_beg_end(medium_kvitable):
    r = medium_kvitable.get_entries_matching(compiler='clang7', optimization=1)
    assert [ ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)], 'good'),
    ] == r

def test_medium_kvitable_get_path_badval(medium_kvitable):
    r = medium_kvitable.get_entries_matching(debug='what')
    assert [] == r

def test_medium_kvitable_get_path_badkey(medium_kvitable):
    r = medium_kvitable.get_entries_matching(debugging='no')
    assert [ ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 0)], 'good'),
             ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 3)], 'ugly'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 0)], 'bad'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 0)], 'good'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 1)], 'bad'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 3)], True),
             ([('compiler', 'clang6'), ('debug', 'yes'), ('optimization', 0)], 'ok'),
             ([('compiler', 'clang7'), ('debug', 'yes'), ('optimization', 3)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 0)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 3)], 'good'),
    ] == r

def test_medium_kvitable_get_no_path(medium_kvitable):
    r = medium_kvitable.get_entries_matching()
    assert [ ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 0)], 'good'),
             ([('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 3)], 'ugly'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 0)], 'bad'),
             ([('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 0)], 'good'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 1)], 'bad'),
             ([('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 3)], True),
             ([('compiler', 'clang6'), ('debug', 'yes'), ('optimization', 0)], 'ok'),
             ([('compiler', 'clang7'), ('debug', 'yes'), ('optimization', 3)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 0)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)], 'good'),
             ([('compiler', 'clang7'), ('debug', 'no'), ('optimization', 3)], 'good'),
    ] == r

def test_medium_kvitable_render_skip_blank_rows(medium_kvitable):
    show = medium_kvitable.render()
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        '|   clang7 |   yes |            3 |  good |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False)
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        '|          |   yes |            3 |  good |',
        '|     gcc7 |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '|          |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep_group_unknown(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False, row_group=['unknown'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        '|          |   yes |            3 |  good |',
        '|     gcc7 |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '|          |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep_group_first(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False, row_group=['compiler'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '+----------+-------+--------------+-------+',
        '|   clang7 |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        '|          |   yes |            3 |  good |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '|          |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '+----------+-------+--------------+-------+',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        '+----------+-------+--------------+-------+',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_group_first_second_unknown(medium_kvitable):
    show = medium_kvitable.render(row_group=['unknown', 'compiler', 'unk', 'debug', 'huh'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '+----------+-------+--------------+-------+',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        '|          +-------+--------------+-------+',
        '|   clang7 |   yes |            3 |  good |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|          +-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '+----------+-------+--------------+-------+',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        '+----------+-------+--------------+-------+',
        ]) == show

def test_medium_kvitable_render_show_blank_rows(medium_kvitable):
    show = medium_kvitable.render(hide_blank_rows=False)
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |    no |            0 |       |',
        '|   clang6 |    no |            1 |       |',
        '|   clang6 |    no |            3 |       |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang6 |   yes |            1 |       |',
        '|   clang6 |   yes |            3 |       |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        '|   clang7 |   yes |            0 |       |',
        '|   clang7 |   yes |            1 |       |',
        '|   clang7 |   yes |            3 |  good |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc7 |    no |            3 |       |',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            1 |       |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc8 |    no |            0 |       |',
        '|     gcc8 |    no |            1 |       |',
        '|     gcc8 |    no |            3 |       |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_unknown(medium_kvitable):
    show = medium_kvitable.render(colstack_at='unknown')
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        '|   clang7 |   yes |            3 |  good |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization')
    assert '\n'.join([
        '| compiler | debug |    0 |    1 |    3 | <- optimization',
        '+----------+-------+------+------+------+',
        '|   clang6 |   yes |   ok |      |      |',
        '|   clang7 |   yes |      |      | good |',
        '|   clang7 |    no | good | good | good |',
        '|     gcc7 |    no |  bad | good |      |',
        '|     gcc7 |   yes | good |      | ugly |',
        '|     gcc8 |   yes | good |  bad | good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization',
                                  row_repeat=False,
                                  row_group=['compiler'],
    )
    assert '\n'.join([
        '| compiler | debug |    0 |    1 |    3 | <- optimization',
        '+----------+-------+------+------+------+',
        '|   clang6 |   yes |   ok |      |      |',
        '+----------+-------+------+------+------+',
        '|   clang7 |    no | good | good | good |',
        '|          |   yes |      |      | good |',
        '+----------+-------+------+------+------+',
        '|     gcc7 |    no |  bad | good |      |',
        '|          |   yes | good |      | ugly |',
        '+----------+-------+------+------+------+',
        '|     gcc8 |   yes | good |  bad | True |',
        '+----------+-------+------+------+------+',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last_val_entry_adj(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization',
                                  row_repeat=False,
                                  row_group=['compiler'],
                                  valstr=lambda v: '[' + (v if isinstance(v, str) else str(v)) + ']',
                                  entrystr=lambda p,e: '<' + (e if isinstance(e, str) else str(e)) + '>',
    )
    assert '\n'.join([
        '| [compiler] | [debug] |    [0] |    [1] |    [3] | <- optimization',
        '+------------+---------+--------+--------+--------+',
        '|   [clang6] |   [yes] |   <ok> |     <> |     <> |',
        '+------------+---------+--------+--------+--------+',
        '|   [clang7] |    [no] | <good> | <good> | <good> |',
        '|         [] |   [yes] |     <> |     <> | <good> |',
        '+------------+---------+--------+--------+--------+',
        '|     [gcc7] |    [no] |  <bad> | <good> |     <> |',
        '|         [] |   [yes] | <good> |     <> | <ugly> |',
        '+------------+---------+--------+--------+--------+',
        '|     [gcc8] |   [yes] | <good> |  <bad> | <True> |',
        '+------------+---------+--------+--------+--------+',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last_val_entry_adj_show_path(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization',
                                  row_repeat=False,
                                  row_group=['compiler'],
                                  valstr=lambda v: '[' + (v if isinstance(v, str) else str(v)) + ']',
                                  entrystr=lambda p,e: str(p) + '<' + (e if isinstance(e, str) else str(e)) + '>',
    )
    print(show)
    assert '\n'.join([
        "| [compiler] | [debug] |                                                                  [0] |                                                                  [1] |                                                                   [3] | <- optimization",
        "+------------+---------+----------------------------------------------------------------------+----------------------------------------------------------------------+-----------------------------------------------------------------------+",
        "|   [clang6] |   [yes] |  [('compiler', 'clang6'), ('debug', 'yes'), ('optimization', 0)]<ok> |    [('compiler', 'clang6'), ('debug', 'yes'), ('optimization', 1)]<> |     [('compiler', 'clang6'), ('debug', 'yes'), ('optimization', 3)]<> |",
        "+------------+---------+----------------------------------------------------------------------+----------------------------------------------------------------------+-----------------------------------------------------------------------+",
        "|   [clang7] |    [no] | [('compiler', 'clang7'), ('debug', 'no'), ('optimization', 0)]<good> | [('compiler', 'clang7'), ('debug', 'no'), ('optimization', 1)]<good> |  [('compiler', 'clang7'), ('debug', 'no'), ('optimization', 3)]<good> |",
        "|         [] |   [yes] |    [('compiler', 'clang7'), ('debug', 'yes'), ('optimization', 0)]<> |    [('compiler', 'clang7'), ('debug', 'yes'), ('optimization', 1)]<> | [('compiler', 'clang7'), ('debug', 'yes'), ('optimization', 3)]<good> |",
        "+------------+---------+----------------------------------------------------------------------+----------------------------------------------------------------------+-----------------------------------------------------------------------+",
        "|     [gcc7] |    [no] |    [('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 0)]<bad> |   [('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 1)]<good> |        [('compiler', 'gcc7'), ('debug', 'no'), ('optimization', 3)]<> |",
        "|         [] |   [yes] |  [('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 0)]<good> |      [('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 1)]<> |   [('compiler', 'gcc7'), ('debug', 'yes'), ('optimization', 3)]<ugly> |",
        "+------------+---------+----------------------------------------------------------------------+----------------------------------------------------------------------+-----------------------------------------------------------------------+",
        "|     [gcc8] |   [yes] |  [('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 0)]<good> |   [('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 1)]<bad> |   [('compiler', 'gcc8'), ('debug', 'yes'), ('optimization', 3)]<True> |",
        "+------------+---------+----------------------------------------------------------------------+----------------------------------------------------------------------+-----------------------------------------------------------------------+",
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_two(medium_kvitable):
    show = medium_kvitable.render(colstack_at='debug')
    assert '\n'.join([
        '| compiler | _______ no _______ | ______ yes _______ | <- debug',
        '|          |    0 |    1 |    3 |    0 |    1 |    3 | <- optimization',
        '+----------+------+------+------+------+------+------+',
        '|   clang6 |      |      |      |   ok |      |      |',
        '|   clang7 | good | good | good |      |      | good |',
        '|     gcc7 |  bad | good |      | good |      | ugly |',
        '|     gcc8 |      |      |      | good |  bad | True |',
        ]) == show

def test_table_printer():
    # Functional equivalence to https://pypi.org/project/table_printer
    # delta:
    #  * KVITable does not support left/right/center alignment
    #  * KVITable does not support explicit field widths
    kvit = KVITable(valuecol_name='Description')
    kvit.add('World', Title='Hello')
    kvit.add('1 title', Title='TOTAL')
    show = kvit.render(row_group=['Title'])
    assert '\n'.join([
        '| Title | Description |',
        '+-------+-------------+',
        '| Hello |       World |',
        '+-------+-------------+',
        '| TOTAL |     1 title |',
        '+-------+-------------+',
    ]) == show

def test_pretty_table_printer():
    # Functional equivalence to
    # https://pypi.org/project/pretty-table-printer
    # https://github.com/cfmeyers/pretty_table_printer
    # (tests/test_pretty_table_printer.py
    # test_it_knows_how_to_print_itself)
    #
    # delta:
    #  * KVITable doesn't have explicit field widths/truncation (implicit in pretty_table_printer?)
    #  * KVITable default alignment is right instead of left
    #  * KVITable doesn't show a bottom border
    kvit = KVITable(valuecol_name='name')
    kvit.add('Sam', id=1)
    kvit.add('Layla', id=2)
    kvit.add('Jack Gabriel', id=3)
    rows = kvit.get_rows()
    assert [ [ 1, 'Sam' ],
             [ 2, 'Layla' ],
             [ 3, 'Jack Gabriel' ],
    ] == rows
    show = kvit.render()
    assert '\n'.join([
        '| id |         name |',
        '+----+--------------+',
        '|  1 |          Sam |',
        '|  2 |        Layla |',
        '|  3 | Jack Gabriel |',
    ]) == show

def test_ptable():
    # Functional equivalence to https://pypi.org/project/PTable
    #
    # delta:
    #  * KVITable default alignment is right instead of left
    kvit = KVITable(valuecol_name='Annual Rainfall')
    kvit.add(600.5, ('City name', 'Adelaide'), Area=1295, Population=1158259)
    kvit.add(1146.4, ('City name', 'Brisbane'), Area=5905, Population=1857594)
    kvit.add(1714.7, ('City name', 'Darwin'), Area=112, Population=120900)
    kvit.add(619.5, ('City name', 'Hobart'), Area=1357, Population=205556)
    kvit.add(646.9, ('City name', 'Melbourne'), Area=1566, Population=3806092)
    kvit.add(869.4, ('City name', 'Perth'), Area=5386, Population=1554769)
    kvit.add(1214.8, ('City name', 'Sydney'), Area=2058, Population=4336374)
    show = kvit.render()
    print(show)
    assert '\n'.join([
        '| City name | Area | Population | Annual Rainfall |',
        '+-----------+------+------------+-----------------+',
        '|  Adelaide | 1295 |    1158259 |           600.5 |',
        '|  Brisbane | 5905 |    1857594 |          1146.4 |',
        '|    Darwin |  112 |     120900 |          1714.7 |',
        '|    Hobart | 1357 |     205556 |           619.5 |',
        '| Melbourne | 1566 |    3806092 |           646.9 |',
        '|     Perth | 5386 |    1554769 |           869.4 |',
        '|    Sydney | 2058 |    4336374 |          1214.8 |',
    ]) == show

@pytest.fixture()
def zoo_table():
    kvit = KVITable({'Location': ['San Diego', 'LA', 'Miami', 'New York'],
                     'Biome': ['Savannah', 'Jungle', 'Polar'],
                     'Category': ['Animal', 'Reptile', 'Bird'],
                     'Diet': ['Herbivore', 'Carnivore'],
                     'Name': [],
                     },
                    valuecol_name='Count',
                    default_factory=int,
                    kv_frozen=False)
    inc = lambda v: v+1
    kvit.add(3, ('Diet', 'Carnivore'), ('Category', "Animal"), ('Biome', "Savannah"), Name='Lion', Location='New York')
    kvit.add(2, ('Diet', 'Carnivore'), ('Category', "Animal"), ('Biome', "Savannah"), Name='Lion', Location='Miami')
    kvit.add(4, ('Diet', 'Carnivore'), ('Category', "Animal"), ('Biome', "Savannah"), Name='Lion', Location='LA')
    kvit.add(8, ('Diet', 'Carnivore'), ('Category', "Animal"), ('Biome', "Savannah"), Name='Lion', Location='San Diego')
    kvit.add(2, Location='LA', Biome='Savannah', Category='Animal', Name='Giraffe', Diet='Herbivore')
    kvit.add(1, Location='LA', Biome='Jungle', Category='Animal', Name='Hippo', Diet='Herbivore')
    kvit.add(3, Location='LA', Biome='Savannah', Category='Animal', Diet='Herbivore', Name='Rhino')
    kvit.add(20, Location='Miami', Biome='Polar', Category='Bird', Diet='Carnivore', Subtype='Gentoo', Name='Penguin')
    kvit.add(8, Location='San Diego', Biome='Polar', Category='Bird', Diet='Carnivore', Subtype='Emperor', Name='Penguin')
    kvit.add(2, Location='San Diego', Biome='Polar', Category='Bird', Diet='Carnivore', Subtype='Gentoo', Name='Penguin')
    kvit.add(3, Location='Miami', Biome='Savannah', Category='Animal', Diet='Herbivore', Name='Giraffe', Subtype='Reticulated')
    kvit.add(inc, Category='Animal', Diet='Carnivore', Biome="Savannah", Location='San Diego', Name='Lion')
    kvit.add(inc, Location='San Diego', Biome='Polar', Category='Animal', Subtype='Polar', Name='Bear', Diet='Omnivore')
    kvit.add(inc, Location='San Diego', Biome='Jungle', Category='Animal', Subtype='Sun', Name='Bear', Diet='Omnivore')
    kvit.add(inc, Location='San Diego', Biome='Plains', Category='Animal', Subtype='Brown', Name='Bear', Diet='Omnivore')
    kvit.add(inc, Location='San Diego', Biome='Plains', Category='Animal', Subtype='Black', Name='Bear', Diet='Omnivore')
    return kvit

def test_zoo_default_factory(zoo_table):
    assert 4 == zoo_table.get( ('Location', 'LA'), Name='Lion', Diet='Carnivore', Category='Animal', Biome='Savannah', Subtype='')
    assert 0 == zoo_table.get( ('Location', 'LA'), Name='Lion', Diet='Carnivore', Category='Animal', Biome='Polar', Subtype='')

def test_zoo_flat_render(zoo_table):
    show = zoo_table.render(row_repeat=False, row_group=['Location', 'Biome', 'Category'])
    assert '\n'.join([
        '|  Location |    Biome | Category |      Diet |    Name |     Subtype | Count |',
        '+-----------+----------+----------+-----------+---------+-------------+-------+',
        '|        LA |   Jungle |   Animal | Herbivore |   Hippo |             |     1 |',
        '|           +----------+----------+-----------+---------+-------------+-------+',
        '|           | Savannah |   Animal | Carnivore |    Lion |             |     4 |',
        '|           |          |          | Herbivore | Giraffe |             |     2 |',
        '|           |          |          |           |   Rhino |             |     3 |',
        '+-----------+----------+----------+-----------+---------+-------------+-------+',
        '|     Miami |    Polar |     Bird | Carnivore | Penguin |      Gentoo |    20 |',
        '|           +----------+----------+-----------+---------+-------------+-------+',
        '|           | Savannah |   Animal | Carnivore |    Lion |             |     2 |',
        '|           |          |          | Herbivore | Giraffe | Reticulated |     3 |',
        '+-----------+----------+----------+-----------+---------+-------------+-------+',
        '|  New York | Savannah |   Animal | Carnivore |    Lion |             |     3 |',
        '+-----------+----------+----------+-----------+---------+-------------+-------+',
        '| San Diego |   Jungle |   Animal |  Omnivore |    Bear |         Sun |     1 |',
        '|           +----------+----------+-----------+---------+-------------+-------+',
        '|           |   Plains |   Animal |  Omnivore |    Bear |       Black |     1 |',
        '|           |          |          |           |         |       Brown |     1 |',
        '|           +----------+----------+-----------+---------+-------------+-------+',
        '|           |    Polar |   Animal |  Omnivore |    Bear |       Polar |     1 |',
        '|           |          +----------+-----------+---------+-------------+-------+',
        '|           |          |     Bird | Carnivore | Penguin |     Emperor |     8 |',
        '|           |          |          |           |         |      Gentoo |     2 |',
        '|           +----------+----------+-----------+---------+-------------+-------+',
        '|           | Savannah |   Animal | Carnivore |    Lion |             |     9 |',
        '+-----------+----------+----------+-----------+---------+-------------+-------+',
    ]) == show

def test_zoo_no_subtype_colstack_render(zoo_table):
    kv = zoo_table.keyvals()
    subtype_idx = list(kv.keys()).index('Subtype')
    del kv['Subtype']
    zt2 = KVITable(kv,
                   valuecol_name='Count',
                   default_factory=int,
                   kv_frozen=False)
    for row in zoo_table.get_rows():
        del row[subtype_idx]
        zt2.add(lambda v: v + row[-1], *tuple(zip(kv, row[:-1])))
    show = zt2.render(row_repeat=False, row_group=['Location', 'Biome', 'Category'], colstack_at='Name')
    assert '\n'.join([
        '|  Location |    Biome | Category |      Diet | Bear | Giraffe | Hippo | Lion | Penguin | Rhino | <- Name',
        '+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|        LA |   Jungle |   Animal | Herbivore |      |         |     1 |      |         |       |',
        '|           +----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|           | Savannah |   Animal | Carnivore |      |         |       |    4 |         |       |',
        '|           |          |          | Herbivore |      |       2 |       |      |         |     3 |',
        '+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|     Miami |    Polar |     Bird | Carnivore |      |         |       |      |      20 |       |',
        '|           +----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|           | Savannah |   Animal | Carnivore |      |         |       |    2 |         |       |',
        '|           |          |          | Herbivore |      |       3 |       |      |         |       |',
        '+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|  New York | Savannah |   Animal | Carnivore |      |         |       |    3 |         |       |',
        '+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '| San Diego |   Jungle |   Animal |  Omnivore |    1 |         |       |      |         |       |',
        '|           +----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|           |   Plains |   Animal |  Omnivore |    2 |         |       |      |         |       |',
        '|           +----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|           |    Polar |   Animal |  Omnivore |    1 |         |       |      |         |       |',
        '|           |          +----------+-----------+------+---------+-------+------+---------+-------+',
        '|           |          |     Bird | Carnivore |      |         |       |      |      10 |       |',
        '|           +----------+----------+-----------+------+---------+-------+------+---------+-------+',
        '|           | Savannah |   Animal | Carnivore |      |         |       |    9 |         |       |',
        '+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+',
    ]) == show


@pytest.fixture()
def build_kvitable():
    kvit = KVITable({'system': [ 'x86_64-linux', 'x86_64-darwin' ],
                     'Branch': ['master', 'develop', 'PR-feature'],
                     'Strategy': ['submodules', 'HEADs'],
                     'ghcver': ['ghc844', 'ghc865', 'ghc882'],
                     },
                    kv_frozen=True)
    kvit.add('+', system='x86_64-linux', Strategy='submodules', ghcver='ghc844', Branch='PR-feature')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='HEADs', ghcver='ghc865', Branch='PR-feature')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='submodules', ghcver='ghc882', Branch='develop')
    kvit.add('+', system='x86_64-linux', Strategy='HEADs', ghcver='ghc865', Branch='develop')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='submodules', ghcver='ghc844', Branch='master')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='HEADs', ghcver='ghc844', Branch='master')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='HEADs', ghcver='ghc865', Branch='master')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='HEADs', ghcver='ghc882', Branch='master')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='submodules', ghcver='ghc865', Branch='master')
    kvit.add('+', system='x86_64-linux', Strategy='HEADs', ghcver='ghc844', Branch='develop')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='HEADs', ghcver='ghc882', Branch='PR-feature')
    kvit.add('FAIL*1', system='x86_64-linux', Strategy='submodules', ghcver='ghc882', Branch='PR-feature')
    kvit.add('+', system='x86_64-linux', Strategy='submodules', ghcver='ghc865', Branch='PR-feature')
    kvit.add('+', system='x86_64-linux', Strategy='submodules', ghcver='ghc844', Branch='develop')
    kvit.add('+', system='x86_64-linux', Strategy='HEADs', ghcver='ghc844', Branch='develop')
    kvit.add('+', system='x86_64-darwin', Strategy='HEADs', ghcver='ghc844', Branch='develop')
    return kvit

def test_no_seplines_on_multirow_non_grouped(build_kvitable):
    show = build_kvitable.render(row_repeat=False, row_group=['Branch'], colstack_at='ghcver')
    assert '\n'.join([
        '|        system |     Branch |   Strategy | ghc844 | ghc865 | ghc882 | <- ghcver',
        '+---------------+------------+------------+--------+--------+--------+',
        '| x86_64-darwin |    develop |      HEADs |      + |        |        |',
        '|               +------------+------------+--------+--------+--------+',
        '|  x86_64-linux | PR-feature |      HEADs |        | FAIL*1 | FAIL*1 |',
        '|               |            | submodules |      + |      + | FAIL*1 |',
        '|               +------------+------------+--------+--------+--------+',
        '|               |    develop |      HEADs |      + |      + |        |',
        '|               |            | submodules |      + |        | FAIL*1 |',
        '|               +------------+------------+--------+--------+--------+',
        '|               |     master |      HEADs | FAIL*1 | FAIL*1 | FAIL*1 |',
        '|               |            | submodules | FAIL*1 | FAIL*1 |        |',
        '|               +------------+------------+--------+--------+--------+',
        # Note ^^^^ no seplines under system because it wasn't included in the row_group
    ]) == show

def test_no_seplines_on_repeated_non_grouped(build_kvitable):
    show = build_kvitable.render(row_repeat=True, row_group=['Branch'], colstack_at='ghcver')
    assert '\n'.join([
        '|        system |     Branch |   Strategy | ghc844 | ghc865 | ghc882 | <- ghcver',
        '+---------------+------------+------------+--------+--------+--------+',
        '| x86_64-darwin |    develop |      HEADs |      + |        |        |',
        '|               +------------+------------+--------+--------+--------+',
        '|  x86_64-linux | PR-feature |      HEADs |        | FAIL*1 | FAIL*1 |',
        '|  x86_64-linux | PR-feature | submodules |      + |      + | FAIL*1 |',
        '|               +------------+------------+--------+--------+--------+',
        '|  x86_64-linux |    develop |      HEADs |      + |      + |        |',
        '|  x86_64-linux |    develop | submodules |      + |        | FAIL*1 |',
        '|               +------------+------------+--------+--------+--------+',
        '|  x86_64-linux |     master |      HEADs | FAIL*1 | FAIL*1 | FAIL*1 |',
        '|  x86_64-linux |     master | submodules | FAIL*1 | FAIL*1 |        |',
        '|               +------------+------------+--------+--------+--------+',
        # Note ^^^^ no seplines under system because it wasn't included in the row_group
    ]) == show
