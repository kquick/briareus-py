# This is the primary external API for repositories

from thespian.actors import ActorSystem
from Briareus.VCS.InternalOps import *
from datetime import timedelta
from typing import (Any, Dict, List, Type, TypeVar)


REPO_INFO_TIMEOUT = timedelta(seconds=600)

def gather_repo_info(RL: List[RepoDesc],
                     RX: List[RepoLoc],
                     BL: List[BranchDesc],
                     actor_system: Any = None) -> Dict[str, InfoReturnTy]:
    """Gets the full set of information for the listed repositories, with
       location translations and branches of interest.
    """
    rspobj = _run_actors(GatherInfo(RL, RX, BL), GatheredInfo, actor_system)
    if rspobj.error:
        raise RuntimeError('VCS request error: ' + str(rspobj.error))
    return { "pullreqs" : rspobj.pullreqs,
             "submodules": rspobj.submodules,
             "subrepos" : rspobj.subrepos,
             "branches" : rspobj.branches  # [ Types.BranchRef ]
    }


def get_updated_file(repourl: UserURL,
                     filepath: str,
                     repolocs: List[RepoLoc],
                     branch: str,
                     actor_system: Any = None) -> FileReadData:
    "Reads a specific file from the repository at the specified url"
    return _run_actors(ReadFileFromVCS("reponame", repourl, repolocs, filepath, branch),
                       FileReadData, actor_system)



VCSActorResponseTy = TypeVar("VCSActorResponseTy") #, Union[GatheredInfo, FileReadData])

def _run_actors(request,
                expected_resp_type: Type[VCSActorResponseTy],
                actor_system=None) -> VCSActorResponseTy:
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
            return rspobj
        raise RuntimeError('Unexpected response to VCS request: %s' % str(rsp))
    finally:
        if actor_system is None:
            asys.shutdown()
