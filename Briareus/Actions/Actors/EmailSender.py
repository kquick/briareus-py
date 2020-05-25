from thespian.actors import *
from Briareus.Actions.Actors.Msgs import *
from Briareus.Actions.Actors.RateLimiter import RateLimiter
import platform
import smtplib
import logging
import os


class EmailSender(ActorTypeDispatcher):
    """Actor to perform actual SMTP send operations, with rate limiting per email address.

       Controls:

        ${BRIAREUS_SMTP} -- not set, no emails sent,
                            "1" send to any target
                            comma-separated list of sendable targets

        ${BRIAREUS_SMTP_SERVER} -- name of SMTP host (default=localhost)

        ${BRIAREUS_SMTP_PORT} -- port of SMTP service (default=0 ... for SMTP default)

        ${BRIAREUS_SMTP_PER_HOUR} -- max emails per hour per target (default=3)

        ${BRIAREUS_SMTP_PER_DAY} -- max emails per day per target (default=10)

        ${BRIAREUS_SMTP_UNLIMITED} -- comma-separated list of targets not subject to rate limits

    """

    def __init__(self, *args, **kw):
        super(EmailSender, self).__init__(*args, **kw)
        self.server = os.getenv('BRIAREUS_SMTP_SERVER', 'localhost')
        port = os.getenv('BRIAREUS_SMTP_PORT', '0')
        try:
            port = int(port)
        except Exception:
            logging.error('Unable to identify EmailSender SMTP port: using 0 (default)',
                          show_exc=True)
            port = 0
        self.port = port
        self.can_email = os.getenv('BRIAREUS_SMTP', None)
        try:
            hlimit = int(os.getenv('BRIAREUS_SMTP_PER_HOUR', '3'))
        except Exception:
            hlimit = 3
        try:
            dlimit = int(os.getenv('BRIAREUS_SMTP_PER_DAY', '10'))
        except Exception:
            dlimit = 10
        unlimited = os.getenv('BRIAREUS_SMTP_UNLIMITED', '').split(',')
        self._delivered = RateLimiter(hourly=hlimit, daily=dlimit, unlimited=unlimited)

    def receiveMsg_str(self, msg, sender):
        if msg == 'Start':
            # Sent by Thespian Director based on the TLI file; this
            # is intended only to ensure this Actor is instantiated.
            pass
        elif msg.startswith('Start:'):
            # The start message overrides ENV variables (useful in
            # situations where the ENV vars are not available, such as
            # systemd + director).
            start, can_email, hval, dval, unlimval, server, port = msg.split()
            self.can_email = can_email
            self.server = server
            try:
                self.port = int(port)
            except Exception:
                logging.error('Error setting port, SMTP port unchanged for EmailSender',
                              show_exc=True)
            try:
                hlimit = int(hval)
                dlimit = int(dval)
                unlimited = '' if unlimval == '-' else unlimval
                self._delivered = RateLimiter(hourly=hlimit, daily=dlimit, unlimited=unlimited)
            except Exception:
                logging.error('Error reconfiguring EmailSender RateLimiter; previous rates still apply',
                              show_exc=True)
        elif msg == 'Deactivate':
            # Sent by Thespian Director prior to shutdown.
            pass
        elif msg == 'status':
            rdict = self._delivered.status()
            rdict.update({'can_email': self.can_email,
                          'server': self.server,
                          'port': self.port,
                          })
            self.send(sender, toJSON(rdict))
        else:
            try:
                objmsg = fromJSON(msg)
            except Exception:
                pass # not sure what this message is; Actor style is to drop it
            else:
                self._dispatch(objmsg, sender, jsonReply=True)

    def _dispatch(self, objmsg, sender, jsonReply=False):
        if isinstance(objmsg, EmailEnvelope):
            self._do_send(objmsg, sender, jsonReply=jsonReply)
        else:
            logging.warning('No handling for objmsg [%s]: %s', type(objmsg), msg)


    def receiveMsg_EmailEnvelope(self, envelope, sender):
        self._do_send(envelope, sender, False)

    def _do_send(self, envelope, sender, jsonReply):
        fmtReply = toJSON if jsonReply else lambda x: x
        send_to = self._delivered.allowed(envelope.subject, envelope.recipients)
        message = envelope.message
        if not self.can_email:
            logging.warn('email to %s suppressed: %s', send_to, envelope.subject)
            self.send(sender, fmtReply(SendReceipt(envelope, envelope.recipients)))
            return
        if self.can_email != "1":
            message = "::: Originally to: " + str(send_to) + "\n\n" + message
            send_to = self.can_email.split(',')
            message = '\n'.join([ 'From: ' + platform.node() + ' Briareus <noreply@noreply.noreply>',
                                  'To: ' + ', '.join(send_to),
                                  'Subject: ' + envelope.subject,
            ]) + "\n\n" + message
        smtp = smtplib.SMTP(self.server, self.port)
        smtp.sendmail('briareus', send_to, message)
        self.send(sender, fmtReply(SendReceipt(envelope, envelope.recipients)))
