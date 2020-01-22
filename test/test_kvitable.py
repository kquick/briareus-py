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

def test_non_leaf_kvitable_add_refused():
    kvit = KVITable()
    kvit.add("hi", ('moo', "cow"), foo='bar')
    with pytest.raises(IndexError) as idxerr:
        kvit.add("oops", ('moo', "dog"))
    assert 'non-leaf' in str(idxerr.value)
    assert 'foo' in str(idxerr.value)

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
        [ 'gcc7', 'yes', 0, 'good' ],
        [ 'gcc7', 'yes', 3, 'ugly' ],
        [ 'gcc7', 'no', 0, 'bad' ],
        [ 'gcc7', 'no', 1, 'good' ],
        [ 'gcc8', 'yes', 0, 'good' ],
        [ 'gcc8', 'yes', 1, 'bad' ],
        [ 'gcc8', 'yes', 3, True ],
        [ 'clang6', 'yes', 0, 'ok' ],
        [ 'clang7', 'yes', 3, 'good' ],
        [ 'clang7', 'no', 0, 'good' ],
        [ 'clang7', 'no', 1, 'good' ],
        [ 'clang7', 'no', 3, 'good' ],
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
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |   yes |            3 |  good |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False)
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '|          |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |   yes |            3 |  good |',
        '|          |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep_group_unknown(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False, row_group=['unknown'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '|          |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |   yes |            3 |  good |',
        '|          |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_no_rep_group_first(medium_kvitable):
    show = medium_kvitable.render(row_repeat=False, row_group=['compiler'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|          |       |            3 |  ugly |',
        '|          |    no |            0 |   bad |',
        '|          |       |            1 |  good |',
        '+----------+-------+--------------+-------+',
        '|     gcc8 |   yes |            0 |  good |',
        '|          |       |            1 |   bad |',
        '|          |       |            3 |  True |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '+----------+-------+--------------+-------+',
        '|   clang7 |   yes |            3 |  good |',
        '|          |    no |            0 |  good |',
        '|          |       |            1 |  good |',
        '|          |       |            3 |  good |',
        '+----------+-------+--------------+-------+',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_group_first_second_unknown(medium_kvitable):
    show = medium_kvitable.render(row_group=['unknown', 'compiler', 'unk', 'debug', 'huh'])
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|          +-------+--------------+-------+',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '+----------+-------+--------------+-------+',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        '+----------+-------+--------------+-------+',
        '|   clang6 |   yes |            0 |    ok |',
        '+----------+-------+--------------+-------+',
        '|   clang7 |   yes |            3 |  good |',
        '|          +-------+--------------+-------+',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        '+----------+-------+--------------+-------+',
        ]) == show

def test_medium_kvitable_render_show_blank_rows(medium_kvitable):
    show = medium_kvitable.render(hide_blank_rows=False)
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            1 |       |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc7 |    no |            3 |       |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        '|     gcc8 |    no |            0 |       |',
        '|     gcc8 |    no |            1 |       |',
        '|     gcc8 |    no |            3 |       |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang6 |   yes |            1 |       |',
        '|   clang6 |   yes |            3 |       |',
        '|   clang6 |    no |            0 |       |',
        '|   clang6 |    no |            1 |       |',
        '|   clang6 |    no |            3 |       |',
        '|   clang7 |   yes |            0 |       |',
        '|   clang7 |   yes |            1 |       |',
        '|   clang7 |   yes |            3 |  good |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_unknown(medium_kvitable):
    show = medium_kvitable.render(colstack_at='unknown')
    assert '\n'.join([
        '| compiler | debug | optimization | Value |',
        '+----------+-------+--------------+-------+',
        '|     gcc7 |   yes |            0 |  good |',
        '|     gcc7 |   yes |            3 |  ugly |',
        '|     gcc7 |    no |            0 |   bad |',
        '|     gcc7 |    no |            1 |  good |',
        '|     gcc8 |   yes |            0 |  good |',
        '|     gcc8 |   yes |            1 |   bad |',
        '|     gcc8 |   yes |            3 |  True |',
        '|   clang6 |   yes |            0 |    ok |',
        '|   clang7 |   yes |            3 |  good |',
        '|   clang7 |    no |            0 |  good |',
        '|   clang7 |    no |            1 |  good |',
        '|   clang7 |    no |            3 |  good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization')
    assert '\n'.join([
        '| compiler | debug |    0 |    1 |    3 | <- optimization',
        '+----------+-------+------+------+------+',
        '|     gcc7 |   yes | good |      | ugly |',
        '|     gcc7 |    no |  bad | good |      |',
        '|     gcc8 |   yes | good |  bad | good |',
        '|   clang6 |   yes |   ok |      |      |',
        '|   clang7 |   yes |      |      | good |',
        '|   clang7 |    no | good | good | good |',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_last(medium_kvitable):
    show = medium_kvitable.render(colstack_at='optimization',
                                  row_repeat=False,
                                  row_group=['compiler'],
    )
    assert '\n'.join([
        '| compiler | debug |    0 |    1 |    3 | <- optimization',
        '+----------+-------+------+------+------+',
        '|     gcc7 |   yes | good |      | ugly |',
        '|          |    no |  bad | good |      |',
        '+----------+-------+------+------+------+',
        '|     gcc8 |   yes | good |  bad | True |',
        '+----------+-------+------+------+------+',
        '|   clang6 |   yes |   ok |      |      |',
        '+----------+-------+------+------+------+',
        '|   clang7 |   yes |      |      | good |',
        '|          |    no | good | good | good |',
        '+----------+-------+------+------+------+',
        ]) == show

def test_medium_kvitable_render_skip_blank_rows_stack_two(medium_kvitable):
    show = medium_kvitable.render(colstack_at='debug')
    assert '\n'.join([
        '| compiler | ______ yes _______ | _______ no _______ | <- debug',
        '|          |    0 |    1 |    3 |    0 |    1 |    3 | <- optimization',
        '+----------+------+------+------+------+------+------+',
        '|     gcc7 | good |      | ugly |  bad | good |      |',
        '|     gcc8 | good |  bad | True |      |      |      |',
        '|   clang6 |   ok |      |      |      |      |      |',
        '|   clang7 |      |      | good | good | good | good |',
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
