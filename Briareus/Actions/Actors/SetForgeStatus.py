from thespian.actors import ActorTypeDispatcher
from thespian.troupe import troupe
from Briareus.Actions.Actors.Msgs import *
import logging
import os
import attr
# n.b. RepoAPI_Location is needed for fromJSON decode
from Briareus.VCS.GitForge import GitHubInfo, GitLabInfo, RepoAPI_Location


NUM_RECENT_RESULTS=100  # maximum number of results of set operations to store


class SetForgeStatus(ActorTypeDispatcher):
    """Actor to perform asynchronous parallel updates of the status of a
       commit (usually a Pull Request/Merge Request) to a specified
       Git Forge (e.g. github or gitlab).

       The updates may take a while to make: this is managed
       asynchronously by the Actors in the system.  The setting
       operation is acknowledged to requesting process, but the actual
       forge updates are performed at a later point.

    """

    def __init__(self, *args, **kw):
        super(SetForgeStatus, self).__init__(*args, **kw)
        self._recent_results = []
        self._set_can_post(os.getenv('BRIAREUS_FORGE_STATUS', None))
        self._setter = None


    def _set_can_post(self, can_post):
        self.can_post = can_post
        try:
            self.can_post=int(can_post)
        except Exception:
            pass


    def receiveMsg_str(self, msg, sender):
        # All unrecognized messages are ignored (e.g. messages from
        # the Thespian Director based on the TLI file to ensure
        # startup, etc.).
        if msg == 'status':
            rdict = { 'recent results': self._recent_results,
                      'can post': self.can_post,
                      'setter': self._setter
            }
            self.send(sender, rdict)
        elif msg.startswith('Start:'):
            # The start message overrides ENV variables (useful in
            # situations where the ENV vars are not available, such as
            # systemd + director).
            start, can_post = msg.split()
            self._set_can_post(can_post)
        else:
            try:
                objmsg = fromJSON(msg)
            except Exception as ex:
                pass # not sure what this message is; Actor style is to drop it
            else:
                self._dispatch(objmsg, sender, jsonReply=True)

    def _dispatch(self, objmsg, sender, jsonReply=False):
        if isinstance(objmsg, NewForgeStatus):
            self._do_setforge(objmsg, sender, jsonReply=jsonReply)
        else:
            logging.warning('No handling for objmsg [%s]: %s', type(objmsg), msg)


    def receiveMsg_NewForgeStatus(self, envelope, sender):
        self._do_setforge(envelope, sender, False)


    def _do_setforge(self, envelope, sender, jsonReply):
        fmtReply = normallyToJSON if jsonReply else lambda x: x
        url_and_rev = { (e.tgt_url, e.rev) : e.proj_url
                        # e is RepoURLRevProjURL
                        for e in envelope.url_and_rev }
        done = []

        # Requests are not processed inline here, but are instead
        # passed to a troupe of worker actors to be performed at some
        # future date.  Reaching this point is implied success, even
        # though the actual update may fail.

        if not self.can_post:
            logging.warning('forge status post to %s suppressed: %s [%s]',
                            list(url_and_rev.values()), envelope.desc, envelope.sts)
        else:
            if not self._setter:
                self._setter = self.createActor(SetStatusActor)
            for loc,rev in url_and_rev:
                self.send(self._setter, (loc, rev,
                                         envelope.sts, envelope.desc,
                                         envelope.stsurl, envelope.project,
                                         url_and_rev[(loc,rev)]))
                done.extend(url_and_rev[(loc,rev)])

        self.send(sender, fmtReply(Posted(envelope, done)))


    def receiveMsg_ChildActorExited(self, exitmsg, sender):
        if exitmsg.childAddress == self._setter:
            self._setter = None

    def receiveMsg_ForgeStatusResults(self, msg, sender):
        # Recent results are stored and can be retrieved with a "status" message
        self._recent_results.append(msg)
        self._recent_results = self._recent_results[-NUM_RECENT_RESULTS:]


@troupe()
class SetStatusActor(ActorTypeDispatcher):
    def receiveMsg_tuple(self, msg, sender):
        loc, rev, sts, desc, stsurl, project, stsrepos = msg
        forge = GitForge(loc)
        try:
            logging.debug('*** forge(%s, %s, %s, %s, %s, %s)',sts,desc,rev,stsurl,project,stsrepos)
            res,msg = forge.set_commit_status(sts, desc, rev, stsurl, project)
        except Exception as ex:
            logging.error('posting forge status to %s: %s', str(loc), str(ex))
            res = False
            msg = str(ex)
        self.send(sender, ForgeStatusResults(res is True, project, rev, stsrepos, sts, msg))


class GitForge(object):
    def __init__(self, repoloc):
        self._repoloc = repoloc  # Briareus.VCS.GitForge.RepoAPI_Location
        self._ghinfo = (GitHubInfo(repoloc) if 'github' in repoloc.apiloc else
                        (GitLabInfo(repoloc) if 'gitlab' in repoloc.apiloc else None))
        if not self._ghinfo:
            raise ValueError('Cannot determine type of remote repo at %s'
                             % repoloc)


    def set_commit_status(self, sts, desc, commitref, url='', context_ref=''):
        rval = self._ghinfo.set_commit_status(sts, desc, commitref, url, context_ref)
        if getattr(rval, 'status_code', None) in [ 200, 201 ]:
            return True, str(rval)
        logging.warning('set_commit_sts response: %s',rval)
        return False, str(rval) + ": " + getattr(rval, 'text', '<no further info>')
