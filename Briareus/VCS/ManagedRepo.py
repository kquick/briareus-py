# This is the primary external API for repositories

from thespian.actors import *
from Briareus.VCS.InternalOps import *
from datetime import timedelta


REPO_INFO_TIMEOUT = timedelta(seconds=600)

def gather_repo_info(RL, BL, cachedir, repo_auth, actor_system = None):
    asys = actor_system or ActorSystem('multiprocTCPBase')  # use TCP base for ThespianWatch support.
    try:
        # Use a global name for this actor to re-connect to the existing "daemon"
        rsp = asys.ask(asys.createActor(Briareus.VCS.InternalOps.GatherRepoInfo,
                                        globalName='GatherRepoInfo'),
                       GatherInfo(RL, BL, cachedir, repo_auth),
                       REPO_INFO_TIMEOUT)
        if rsp == None:
            raise RuntimeError('Timeout waiting for GatherInfo response')
        if isinstance(rsp, GatheredInfo):
            if rsp.error:
                raise RuntimeError('GatherRepoInfo error: ' + str(rsp.error))
            return rsp.info
        raise RuntimeError('Unexpected response to GatherInfo request: %s' % str(rsp))
    finally:
        if actor_system is None:
            asys.shutdown()
