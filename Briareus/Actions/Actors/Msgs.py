import attr
import json


def toJSON(obj):
    """Convert class objects to json by providing a type hint that can be
    used by the fromJSON decoder.

    Warning: be careful passing tuple objects!  The python json class
    performs encoding of standard objects and only calls the 'default'
    below if it doesn't have a default encoding.  The default encoding
    for tuples is a list, so on decode, the tuple will have been
    converted to a python list.  In many cases, this is fine, but not
    if the target object is required to be immutable (e.g. as a
    dictionary key)

    """
    class objToJSON(json.JSONEncoder):
        def default(self, obj):
            if obj.__class__.__name__ in ['set']:
                return { '__type__': obj.__class__.__name__,
                         '__value__': [self.default(e) for e in obj]
                }
            if obj.__class__.__name__ == 'datetime':
                return { '__type__': obj.__class__.__name__ + '.fromisoformat',
                         '__value__': str(obj)
                }
            if obj.__class__.__name__ == 'ActorAddress':
                return str(str(obj))
            try:
                objdict = attr.asdict(obj, recurse=False)
                objdict['__type__'] = obj.__class__.__name__
                return objdict
            except Exception:
                return super(objToJSON, self).default(obj)
    return json.dumps(obj, cls=objToJSON)


def normallyToJSON(thing):
    try:
        return toJSON(thing)
    except TypeError:
        return thing


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
