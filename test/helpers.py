from Briareus.KVITable import KVITable


def assert_eq(a, b):
    assert a == b, '%s != %s' % (str(a), str(b))
def assert_in(a, b):
    assert a in b, '%s not in %s' % (str(a), str(b))
def assert_eqlist(a, b):
    c = KVITable({'idx':list(range(max(len(a),len(b)))),
                  'fail':[],
                  'From':['L','R'],
    })
    alst = sorted(list(a))
    blst = sorted(list(b))
    fails = [' ' if i < len(alst) and i < len(blst) and alst[i] == blst[i] else '!'
             for i in range(max(len(a),len(b)))]
    for i,(av,f) in enumerate(zip(alst,fails)):
        c.add(av, idx=i, From='L', fail=f)
    for i,(bv,f) in enumerate(zip(blst,fails)):
        c.add(bv, idx=i, From='R', fail=f)
    assert alst == blst, '\n'+c.render(
        row_group=['idx'],
        row_repeat=False,
        colstack_at=('From' if max(max(map(len,a)),
                                   max(map(len,b))) < 50
                     else '')
    )

def assert_eqdict(a, b, a_name='L', b_name='R'):
    keys = sorted(list(set(a.keys()).union(set(b.keys()))))
    c = KVITable({'key':keys,
                  'fail':[],
                  'From':[a_name, b_name],
    })
    for k in keys:
        fail = '!' if k not in a or k not in b or a[k] != b[k] else ' '
        c.add(a.get(k,' '), key=k, From=a_name, fail=fail)
        c.add(b.get(k,' '), key=k, From=b_name, fail=fail)
    assert a == b, '\n'+c.render(
        row_group=['key'],
        row_repeat=False,
        colstack_at=(
            'From'
            if max(max(list(map(len,map(str,a.values()))) or [0]),
                   max(list(map(len,map(str,b.values()))) or [0])) < 50
            else '')
    )
