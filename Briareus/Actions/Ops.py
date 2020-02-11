"""Primary module implementing Briareus Actions.  Call do_action with
each Report statement; if it is actionable, the action is performed
and the statement is possibly updated to reflect the action.
"""

from Briareus.Actions.SendEmail import do_send_email_action

actions = {
    'SendEmail': do_send_email_action
}

def do_action(possible_action, full_report, inpcfg):
    act = actions.get(possible_action.__class__.__name__, None)
    if not act:
        return possible_action
    return act(possible_action, full_report, inpcfg)
