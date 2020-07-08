import attr
import os
from urllib.parse import urlparse, urlunparse


@attr.s(frozen=True)
class RepoAPI_Location(object):
    apiloc = attr.ib()   # HTTP API URL base used internally to get
                         # information.  This may not yet be a valid
                         # API reference because there is often path
                         # or URL adjustments based on that, but this
                         # should be an http reference to an API
                         # server (e.g. a git forge) instead of an ssh
                         # or http reference to a repository.
    apitoken = attr.ib() # Token used to access the API URL (or None if no token)


def _remove_trailer(path, trailer):
    trailer_len = len(trailer)
    return path[:-trailer_len] if path[-trailer_len:] == trailer else path

def _changeloc(url, repolocs):
    parsed = urlparse(url)
    for each in repolocs:
        if parsed.netloc == each.repo_loc:
            return urlunparse(parsed._replace(netloc=each.api_host)), each.api_host, parsed.netloc
    return url, parsed.netloc, parsed.netloc

def to_http_url(url, repolocs):
    """Converts git clone access specification
    (e.g. "git@foo.com:group/proj") to the corresponding HTTP forge
    reference RepoAPI_URL (e.g. "https://foo.com/group/proj").  Also
    works if the source ends with ".git".

    Performs any network location translations specified in the xlate
    list (which has (from, to) pairs in it as commonly specified by
    the RepoLoc input specification.

    Returns the translated URL along with any access token for that
    URL (as extracted from the BRIAREUS_PAT environment variable).

    """
    if url.startswith("git@"):
        trimmed_url = _remove_trailer(url[len('git@'):], '.git')
        spl = trimmed_url.split(':')
        return to_http_url('https://%s/%s' % (spl[0], ':'.join(spl[1:])), repolocs)

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

def to_access_url(url, for_repo, repolocs): # KWQ: use for_repo to get "git@" portion instead of https portion...
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
        return url

    if not for_repo.repo_url.startswith('git@'):
        # Primary input repo doesn't use SSH access, so presumably
        # repo is publicly accessible and the URL will work
        return url

    # The for_repo specification indicates that SSH access is needed,
    # so extract the hostname so that the same hostname can be used in
    # the input URL to ensure the same Builder's .ssh/config entry is
    # used.

    trimmed_url = _remove_trailer(for_repo.repo_url[len('git@'):], '.git')
    ssh_host = trimmed_url.split(':')[0]

    # The input URL can be in http form or already in ssh form.
    if url.startswith("git@"):
        url_path = _remove_trailer(url[len('git@'):], '.git').split(':')[1]
    else:
        parsed = urlparse(url)
        # n.b. assumes there are no params, query, or fragment
        # portions of a git http URL.  Remove the initial slash on the
        # path part as well.
        url_path = parsed.path[1:]

    return "git@" + ssh_host + ":" + url_path
