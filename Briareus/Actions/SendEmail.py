"""Implementation of Action sending Email.  Uses local SMTP server by
default, although this can be overridden by BRIAREUS_SMTP_SERVER and
BRIAREUS_SMTP_PORT environment variable values.

Because of potential impact, the BRIAREUS_SMTP environment variable
must be set to "1" to enable actual sending of email.  If this
environment variable is not set, email sending is suppressed (and
warnings are generated).  Alternatively, the BRIAREUS_SMTP environment
variable may be set to a comma-separated list of alternate recipients
who will receive all emails (replacing the originally intended
recipients); this is intended for testing mode.

"""

import os
import sys
from datetime import timedelta
from thespian.actors import *
from Briareus.Actions.Content import gen_content
from Briareus.Actions.Actors.Msgs import *
from Briareus.Types import SendEmail

SEND_EMAIL_TIMEOUT = timedelta(seconds=20)


def do_send_email_action(email_action, inpcfg, run_context):
    "Sends email, where email_action is Briareus.Types.SendEmail"
    rec = email_action.recipients
    done = email_action.sent_to
    send_to = sorted(list(set(rec) - set(done)))
    if send_to:
        subj, body = gen_content('email', email_action.notification, run_context)
        if body:
            sent_to = send_email(send_to, subj, body, run_context)
        else:
            # Output is suppressed, so indicate all targets have been satisfied
            sent_to = send_to
        email_action = SendEmail(recipients=email_action.recipients,
                                 notification=email_action.notification,
                                 sent_to = list(set(email_action.sent_to).union(sent_to)))
    return email_action


def send_email(recipients, subject, message, run_context):
    if not run_context.actor_system:
        run_context.actor_system = ActorSystem('multiprocTCPBase')
    asys = run_context.actor_system

    # Use a global name for this actor to re-connect to the existing "daemon"
    rsp = asys.ask(asys.createActor('Briareus.Actions.Actors.EmailSender.EmailSender',
                                    globalName='EmailSender'),
                   toJSON(EmailEnvelope(recipients, subject, message)),
                   SEND_EMAIL_TIMEOUT)
    if rsp == None:
        raise RuntimeError('Timeout waiting for EmailSender response')
    rspobj = fromJSON(rsp)
    if isinstance(rspobj, SendReceipt):
        return rspobj.recipients
    raise RuntimeError('Unexpected response to EmailSender request: %s' % str(rsp))
