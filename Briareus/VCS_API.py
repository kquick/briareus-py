import attr
from typing import Optional


class UserURL(str): 'URL specified by a user, can be any form'


@attr.s(auto_attribs=True)
class SSHHostName(object):
    # Used to maintain the equivalence between the SSH host access
    # name used in a "git@hostname:owner/repo" URL and the HTTPS host
    # name used in an "https://hostname/owner/repo" URL.  The default
    # is a 1:1 equivalence but the former might be different if the
    # ssh/.config file specifies a special hostname entry that is
    # associated with an actual hostname along with a deployment key.
    # The Builder will typically clone the repository using the SSH
    # mode (and associated SSH key) but the Briareus API access must
    # be done via the HTTPS API and associated BRIAREUS_PAT provided
    # api token.
    ssh_hostname: str
    https_hostname: str


@attr.s(auto_attribs=True, frozen=True)
class BranchRef(object):
    reponame: str
    branchname: str
    branchref: str   # reference (e.g. SHA)


@attr.s(auto_attribs=True, frozen=True)
class PullReqStatus__Base(object):
    def as_fact(self) -> str:
        return self.__class__.__name__.lower()
class PRSts_Active(PullReqStatus__Base): pass
class PRSts_Closed(PullReqStatus__Base): pass
class PRSts_Merged(PullReqStatus__Base): pass


@attr.s(auto_attribs=True, frozen=True)
class SubModuleInfo(object):
    """Describes a known submodule for a repository.  Only a project
       repository is checked for submodules.

       Note that a Submodule specifies a subrepo name and version, and
       that since a VCS repository may contain several modules, that a
       single VCS submodule reference might create multiple
       SubModuleInfo objects---one for each module---which differ only
       in sm_sub_name.

       The repo to which this submodule reference belongs is
       identified by the tuple of: repo_name, branch, and pullreq_id.
       Note that any branch or pullreq might change the submodule
       specification.  The pullreq_id may be None, indicating that
       this is a branch in the main project repo; if the pullreq_id is
       not None, it is the ID of the pullreq (one or more pullreq
       source repos might use the same branch name, so the pullreq_id
       is used to uniquely differentiante between these).

    """
    sm_repo_name: str  # Primary repo name (the one that contains the submodule)
    sm_branch: str     # Submodule branch name (in source repo)
    sm_pullreq_id: Optional[str] # None if main repo branch, string if
                                 # submodule for a PR to the project
                                 # repo, this identifies the PR
    sm_sub_name: str   # Submodule repo name
    sm_sub_vers: str   # Submodule repo specified version
