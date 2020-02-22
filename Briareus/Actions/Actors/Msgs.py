import attr
import json


def toJSON(obj):
    class objToJSON(json.JSONEncoder):
        def default(self, obj):
            if obj.__class__.__name__ in [ 'dict', 'list', 'int', 'float',
                                           'str', 'bool', 'NoneType' ]:
                return obj
            if obj.__class__.__name__ in ['set', 'tuple']:
                return { '__type__': obj.__class__.__name__,
                         '__value__': [self.default(e) for e in obj]
                }
            if obj.__class__.__name__ == 'datetime':
                return { '__type__': obj.__class__.__name__ + '.fromisoformat',
                         '__value__': str(obj)
                }
            objdict = attr.asdict(obj, recurse=False)
            objdict['__type__'] = obj.__class__.__name__
            return objdict
    return json.dumps(obj, cls=objToJSON)

def fromJSON(jstr):
    def objFromJSON(objdict):
        if '__type__' in objdict:
            objtype = objdict['__type__']
            if '__value__' in objdict:
                return eval(objtype)(objdict['__value__'])
            del objdict['__type__']
            return eval(objtype)(**objdict)
        return objdict
    return json.loads(jstr, object_hook=objFromJSON)


@attr.s
class EmailEnvelope(object):
    recipients = attr.ib()  # list of strs
    subject = attr.ib() # string subject
    message = attr.ib() # string message


@attr.s
class SendReceipt(object):
    envelope = attr.ib()  # EmailEnvelope
    recipients = attr.ib() # list of strs email was sent successfully to
