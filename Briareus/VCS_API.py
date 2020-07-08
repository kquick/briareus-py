import attr


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
