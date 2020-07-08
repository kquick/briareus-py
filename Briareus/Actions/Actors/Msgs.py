import attr
import json
from typing import (List, TypeVar, Union)
# Bring objects that might appear in the JSON messages into scope
from Briareus.VCS.ForgeAccess import UserURL, RepoAPI_Location


def toJSON(obj) -> str:
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

SomeTy = TypeVar('SomeTy')

def normallyToJSON(thing: SomeTy) -> Union[str, SomeTy]:
    try:
        return toJSON(thing)
    except TypeError:
        return thing


def fromJSON(jstr: str):
    def objFromJSON(objdict):
        if '__type__' in objdict:
            objtype = objdict['__type__']
            if '__value__' in objdict:
                return eval(objtype)(objdict['__value__'])
            del objdict['__type__']
            return eval(objtype)(**objdict)
        return objdict
    return json.loads(jstr, object_hook=objFromJSON)


@attr.s(auto_attribs=True)
class EmailEnvelope(object):
    recipients: List[str]
    subject: str
    message: str


@attr.s(auto_attribs=True)
class SendReceipt(object): # Response to EmailEnvelope
    envelope: EmailEnvelope
    recipients: List[str]


@attr.s(auto_attribs=True)
class RepoURLRevProjURL(object):
    tgt_url: RepoAPI_Location
    rev: str  # str revision hash
    proj_names: List[str]  # string project

@attr.s(auto_attribs=True)
class NewForgeStatus(object):
    url_and_rev: List[RepoURLRevProjURL]
    sts: str         # "pending", "success", "failure"
    desc: str        # text description of status
    stsurl: str      # string URL for status details
    project: UserURL     # project name string

@attr.s(auto_attribs=True)
class Posted(object):      # Response to NewForgeStatus
    envelope: NewForgeStatus
    successful: List[str]  # project names

@attr.s(auto_attribs=True)
class ForgeStatusResults(object):
    succeeded: bool
    project: str
    rev: str     # target VCS revision hash string
    projrepos: List[UserURL] # repo for which these results were set
    status: str  # posted status (string)
    message: str # on failure
