import attr
import os
from urllib.parse import urlparse, urlunparse
from Briareus.VCS_API import RepoSite, SSHHostName, UserURL, SSH_URL
from typing import (Any, List, Optional, Tuple, Type, TypeVar, Union)


class HTTPS_URL(str): 'Form: https://github.com/project/repo'
class API_URL(str): 'URL for accessing the API interface'
class SAME_URL(object):
    "Same URL as referencing URL, but latter isn't known when this is set."

@attr.s(auto_attribs=True, frozen=True)
class DIFFERENT_URL(object):
    """URL is different than the current URL, but there is not enough
       information to get an actual URL.  This can happen for gitforge
       merge request source URLs.
    """
    reponame: str
    urltype: str = 'DURL'

@attr.s(auto_attribs=True, frozen=True)
class BOGUS_URL(object):
    """Not an actual URL but something that was synthesized internally as
       a placeholder.  The placeholder should never actually be used."""
    reason: str
    urltype: str = 'BOGUS'


# Note that only the SSH_URL can contain an alternate hostname that is
# translated by the RX translations.


@attr.s(auto_attribs=True, frozen=True)
class RepoAPI_Location(object):
    apiloc: API_URL  # HTTP API URL base used internally to get
                     # information.  This may not yet be a valid API
                     # reference because there is often path or URL
                     # adjustments based on that, but this should be an
                     # http reference to an API server (e.g. a git forge)
                     # instead of an ssh or http reference to a
                     # repository.
    apitoken: Optional[str]  # Token used to access the API URL (or None if no token)


def _remove_trailer(path: str, trailer: str) -> str:
    trailer_len = len(trailer)
    return path[:-trailer_len] if path[-trailer_len:] == trailer else path


def _changeloc(url: str,
               repolocs: List[SSHHostName]) -> Tuple[API_URL, str, str]: # unchanged
    parsed = urlparse(url)
    for each in repolocs:
        if parsed.netloc == each.ssh_hostname:
            return API_URL(urlunparse(parsed._replace(netloc=each.https_hostname))), each.https_hostname, parsed.netloc
    return API_URL(url), parsed.netloc, parsed.netloc


def to_http_url(url: Union[UserURL, HTTPS_URL, SSH_URL],
                repolocs: List[SSHHostName]) -> RepoAPI_Location:
    """Converts git clone access specification
    (e.g. "git@foo.com:group/proj") to the corresponding HTTP forge
    reference RepoAPI_URL (e.g. "https://foo.com/group/proj").  Also
    works if the source ends with ".git".

    Performs any network location translations specified in the xlate
    list (which has (from, to) pairs in it as commonly specified by
    the SSHHostName input specification.

    Returns the translated URL along with any access token for that
    URL (as extracted from the BRIAREUS_PAT environment variable).

    """
    if isinstance(url, SSH_URL) or url.startswith("git@"):
        trimmed_url = _remove_trailer(url[len('git@'):], '.git')
        spl = trimmed_url.split(':')
        return to_http_url(HTTPS_URL('https://%s/%s' % (spl[0], ':'.join(spl[1:]))),
                           repolocs)

    returl, for_remote, orig_remote_spec = _changeloc(_remove_trailer(url, '.git'), repolocs)

    patspec = os.getenv('BRIAREUS_PAT')
    if patspec is None:
        return RepoAPI_Location(returl, None)
    # The BRIAREUS_PAT format: "remote=PATSPEC;...", where PATSPEC
    # varies by the type of forge.  For Github, the PATSPEC is
    # "user:token".  For Gitlab, the PATSPEC is simply "token".
    patlist = patspec.split(';')

    # First try finding a PAT using the original target specification,
    # in case there is a specific PAT associated with a specific
    # remote.
    for pat in patlist:
        if pat.startswith(orig_remote_spec + '='):
            patval = pat[len(for_remote)+1:]
            return RepoAPI_Location(returl, patval)

    # Now try with the target remote to get a general translation
    for pat in patlist:
        if pat.startswith(for_remote + '='):
            patval = pat[len(for_remote)+1:]
            return RepoAPI_Location(returl, patval)

    return RepoAPI_Location(returl, None)


def to_access_url(url: str,
                  for_repo: Optional[RepoSite],
                  repolocs: List[SSHHostName]) -> Union[UserURL, SSH_URL]:
    """The Repo specification in the input may use a git ssh reference to
       a repo (e.g. "git@myproj-github:foo/bar") which indicates that
       an SSH deploy key is being used by the Builder (e.g. Hydra) to
       access the repository.

       This function should be called with any URL that the builder
       might use to access a version of that repository (e.g. a pull
       request or merge request reference, or a submodule), and it
       will translate the URL *back* into the form that the builder
       will need to use to access that repository.  If no translation
       is needed, the input URL is returned untouched.

    """
    if not for_repo:
        # URL is not for a primary input repo.  It is probably a
        # subrepo.  Because it is not a primary, there is no
        # translation information available.
        return UserURL(url)

    if not for_repo.repo_url.startswith('git@'):
        # Primary input repo doesn't use SSH access, so presumably
        # repo is publicly accessible and the URL will work
        return UserURL(url)

    # The for_repo specification indicates that SSH access is needed,
    # so extract the hostname so that the same hostname can be used in
    # the input URL to ensure the same Builder's .ssh/config entry is
    # used.
    #
    # This is often called for URLs obtained from examining PR
    # sources.  In this case, the URL references the source of the
    # pull request, which is often a fork of the for_repo.  The
    # for_repo may be accessed via SSH, but there is no way to
    # determine whether the fork source needs to be translated or not,
    # and if it does, what the proper translation is, since the forked
    # repo will have its own deploy key and therefore a different ssh
    # access entry.  Because of this ambiguity, the ssh translation is
    # *only* done if the repository is the same (identical path portions).

    trimmed_url = _remove_trailer(for_repo.repo_url[len('git@'):], '.git')
    ssh_host, for_path = trimmed_url.split(':')

    # The input URL can be in http form or already in ssh form.
    if url.startswith("git@"):
        url_path = _remove_trailer(url[len('git@'):], '.git').split(':')[1]
    else:
        parsed = urlparse(url)
        # n.b. assumes there are no params, query, or fragment
        # portions of a git http URL.  Remove the initial slash on the
        # path part as well.
        url_path = parsed.path[1:]

    if url_path != for_path:
        # The input path does not match the for_path, so this is
        # assumed to be a source repo for a PR; don't assume the
        # ssh translation holds for it.
        return UserURL(url)

    return SSH_URL("git@" + ssh_host + ":" + url_path)
