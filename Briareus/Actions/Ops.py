"""Primary module implementing Briareus Actions.  Call do_action with
each Report statement; if it is actionable, the action is performed
and the statement is possibly updated to reflect the action.
"""

from Briareus.Actions import ActionType
from Briareus.Actions.SendEmail import do_send_email_action
from Briareus.Actions.ForgeStatus import do_set_forge_status
from Briareus.State import RunContext, ReportType
from typing import Any, Dict, TypeVar

actions = {
    'SendEmail': do_send_email_action,
    'SetForgeStatus': do_set_forge_status,
}

def do_action(possible_action: ActionType,
              inpcfg: ReportType,
              run_context: RunContext,
              report_supplement: Dict[str, Any]) -> ActionType:
    act = actions.get(possible_action.__class__.__name__, None)
    if not act:
        return possible_action
    return act(possible_action, inpcfg, run_context, report_supplement) # type: ignore
