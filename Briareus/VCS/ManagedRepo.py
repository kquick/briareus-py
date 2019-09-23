# This is the primary external API for repositories

from thespian.actors import *
from Briareus.VCS.InternalOps import *
from datetime import timedelta


REPO_INFO_TIMEOUT = timedelta(seconds=600)

def gather_repo_info(RL, RX, BL, repo_auth, actor_system = None):
    asys = actor_system or ActorSystem('multiprocTCPBase')  # use TCP base for ThespianWatch support.
    try:
        # Use a global name for this actor to re-connect to the existing "daemon"
        rsp = asys.ask(asys.createActor('Briareus.VCS.InternalOps.GatherRepoInfo',
                                        globalName='GatherRepoInfo'),
                       toJSON(GatherInfo(RL, RX, BL, repo_auth)),
                       REPO_INFO_TIMEOUT)
        if rsp == None:
            raise RuntimeError('Timeout waiting for GatherInfo response')
        rspobj = fromJSON(rsp)
        if isinstance(rspobj, GatheredInfo):
            if rspobj.error:
                raise RuntimeError('GatherRepoInfo error: ' + str(rspobj.error))
            return rspobj.info
        raise RuntimeError('Unexpected response to GatherInfo request: %s' % str(rsp))
    finally:
        if actor_system is None:
            asys.shutdown()
