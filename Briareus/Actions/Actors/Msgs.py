import attr
import json

# Bring objects that might appear in the JSON messages into scope
from Briareus.VCS.GitForge import RepoAPI_Location


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
class SendReceipt(object): # Response to EmailEnvelope
    envelope = attr.ib()   # EmailEnvelope
    recipients = attr.ib() # list of strs email was sent successfully to


@attr.s
class RepoURLRevProjURL(object):
    tgt_url  = attr.ib()  # RepoAPI_Location
    rev      = attr.ib()  # str revision hash
    proj_url = attr.ib()  # string project url

@attr.s
class NewForgeStatus(object):
    url_and_rev = attr.ib() # array of RepoURLRevProjURL
    sts = attr.ib()         # "pending", "success", "failure"
    desc = attr.ib()        # text description of status
    stsurl = attr.ib()      # string URL for status details
    project = attr.ib()     # project name string

@attr.s
class Posted(object):      # Response to NewForgeStatus
    envelope = attr.ib()   # NewForgeStatus
    successful = attr.ib() # list of proj_url
