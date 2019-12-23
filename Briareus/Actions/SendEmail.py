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
import platform
import smtplib
import sys
from Briareus.Actions.Content import gen_content
from Briareus.Types import SendEmail

def do_send_email_action(email_action):
    "Sends email, where email_action is Briareus.Types.SendEmail"
    rec = email_action.recipients
    done = email_action.sent_to
    send_to = set(rec) - set(done)
    if send_to:
        subj, body = gen_content('email', email_action.notification)
        sent_to = send_email(send_to, subj, body)
        email_action = SendEmail(recipients=email_action.recipients,
                                 notification=email_action.notification,
                                 sent_to = list(set(email_action.sent_to).union(sent_to)))
    return email_action

def send_email(recipients, subject, message):
    send_to = recipients
    can_email = os.getenv('BRIAREUS_SMTP', None)
    if not can_email:
        print('Warning: email to %s suppressed: %s' % (recipients, subject), file=sys.stderr)
        return recipients
    if can_email != "1":
        send_to = can_email.split(',')
        message = "::: Originally to: " + str(send_to) + "\n\n" + message
    message = '\n'.join([ 'From: ' + platform.node() + ' Briareus <noreply@noreply.noreply>',
                          'To: ' + ', '.join(send_to),
                          'Subject: ' + subject,
    ]) + "\n\n" + message
    try:
        smtp = smtplib.SMTP(os.getenv('BRIAREUS_SMTP_SERVER', 'localhost'),
                            port=int(os.getenv('BRIAREUS_SMTP_PORT', '0')))
        smtp.sendmail('briareus', send_to, message)
        return recipients
    except Exception as ex:
        print('Error sending email to %s: %s' % (str(send_to), str(ex)), file=sys.stderr)
        return []
