# This is the primary external API for repositories

from thespian.actors import *
from Briareus.VCS.InternalOps import *
from datetime import timedelta


REPO_INFO_TIMEOUT = timedelta(seconds=600)

def gather_repo_info(RL, RX, BL, actor_system=None):
    """Gets the full set of information for the listed repositories, with
       location translations and branches of interest.
    """
    return _run_actors(GatherInfo(RL, RX, BL), GatheredInfo, actor_system)


def _run_actors(request, expected_resp_type, actor_system=None):
    asys = actor_system or ActorSystem('multiprocTCPBase')  # use TCP base for ThespianWatch support.
    try:
        # Use a global name for this actor to re-connect to the existing "daemon"
        rsp = asys.ask(asys.createActor('Briareus.VCS.InternalOps.GatherRepoInfo',
                                        globalName='GatherRepoInfo'),
                       toJSON(request),
                       REPO_INFO_TIMEOUT)
        if rsp == None:
            raise RuntimeError('Timeout waiting for GatherInfo response')
        rspobj = fromJSON(rsp)
        if isinstance(rspobj, expected_resp_type):
            if rspobj.error:
                raise RuntimeError('VCS request error: ' + str(rspobj.error))
            return rspobj.info
        raise RuntimeError('Unexpected response to VCS request: %s' % str(rsp))
    finally:
        if actor_system is None:
            asys.shutdown()
