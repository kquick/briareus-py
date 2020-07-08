"Primary module providing generation of content message from Notify logic output"

import sys
import os.path
import Briareus.Actions
from Briareus.Types import Notify, PRFailedSubBlds, PRFailedStdBlds
from Briareus.State import RunContext
from typing import Optional, Tuple


ContentReturnType = Tuple[Optional[str],Optional[str]]


class FileContent(object):
    """Use this module for content which should come from local files.
       The file is searched for with
       "{action_type}notification_type.txt" first (with ".tag" for the
       tagline) and if that doesn't exist, then without the
       "{action_type}".  The contents can refer to the notification as
       {n} for format substitution.

    """
    def __call__(self, action_type: str,
                 notification: Notify,
                 runctxt: RunContext) -> ContentReturnType:
        ntype = notification.what
        fromdir = os.path.dirname(sys.modules['Briareus.Actions'].__file__)
        content_fname = os.path.join(fromdir, '{' + action_type + '}' + ntype + '.txt')
        tagline_fname = os.path.join(fromdir, '{' + action_type + '}' + ntype + '.tag')

        if not os.path.exists(content_fname):
            orig_content_fname = content_fname
            content_fname = os.path.join(fromdir, ntype + '.txt')
        if os.path.exists(content_fname):
            content = open(content_fname, 'r').read().format(n=notification)
        else:
            print('No Content file',content_fname,'or',orig_content_fname,
                  'so no action taken',
                  file=sys.stderr)
            return None, None

        if not os.path.exists(tagline_fname):
            tagline_fname = os.path.join(fromdir, ntype + '.tag')
        if os.path.exists(tagline_fname):
            tagline = open(tagline_fname, 'r').read().format(n=notification)
        else:
            tagline = notification.what
        return tagline, content

class PR_ProjStatus_Fail(object):
    def __call__(self, action_type: str,
                 notification: Notify,
                 runctxt: RunContext) -> ContentReturnType:
        if action_type == 'forge_status':

            project = notification.subject

            # N.B. space is *extremely* limited.  The API accepts
            # about 120 characters, but the display shows even less
            # and the full 120 is only visible on mouse hover.

            pi = notification.params
            if isinstance(pi, PRFailedStdBlds):
                txt = 'Fails {numfail}/{numtotal} builds (master {mastersts})'.format(
                    numfail=len(pi.pr_blds.fails),
                    numtotal=len(pi.pr_blds.fails) + len(pi.pr_blds.goods),
                    mastersts='{numfail}/{numtotal}'.format(
                        numfail=len(pi.main_blds.fails),
                        numtotal=len(pi.main_blds.fails) + len(pi.main_blds.goods),
                    ) if pi.main_blds.fails else 'succeeds'
                )
            elif isinstance(pi, PRFailedSubBlds):
                if (len(pi.pr_subs.goods) + len(pi.pr_subs.fails) ==
                    len(pi.pr_heads.goods) + len(pi.pr_heads.fails)):
                    txt = 'Fails {subfails}/{headfails}/{total} (master {mastersts}) submods/heads/total'.format(
                        subfails=len(pi.pr_subs.fails),
                        headfails=len(pi.pr_heads.fails),
                        total=len(pi.pr_subs.fails) + len(pi.pr_subs.goods),
                        mastersts=('{subfails}/{headfails}/{total}'.format(
                            subfails=len(pi.main_subs.fails),
                            headfails=len(pi.main_heads.fails),
                            total=len(pi.main_subs.fails) + len(pi.main_subs.goods))
                                   if (len(pi.main_subs.fails) + len(pi.main_subs.goods) ==
                                       len(pi.main_heads.fails) + len(pi.main_heads.goods))
                                   else '{subfails}/{subtot} {headfails}/{headtot}'.format(
                                           subfails=len(pi.main_subs.fails),
                                           subtot=len(pi.main_subs.fails) + len(pi.main_subs.goods),
                                           headfails=len(pi.main_heads.fails),
                                           headtot=len(pi.main_heads.fails) + len(pi.main_heads.goods),
                                   )))
                else:
                    summ = lambda b: '{numf}/{numt}'.format(
                        numf=len(b.fails), numt=len(b.goods)+len(b.fails),
                    ) if b.fails else 'succeeds'
                    txt = ('Fails submods={substs}'
                           ', heads={headsts}, (master {mastersts})'.format(
                               substs=summ(pi.pr_subs),
                               headsts=summ(pi.pr_heads),
                               mastersts='submods={substs}, heads={headsts}'.format(
                                   substs=summ(pi.main_subs),
                                   headsts=summ(pi.main_heads),
                               )))
            else:
                raise TypeError('No translation for pr_projstatus_fail param type %s'
                                % type(notification.params))

            return action_type, txt

        return None, None

notify_generators = {
    'variable_failing': FileContent(),
    'completely_broken': FileContent(),
    'pr_projstatus_pending': FileContent(),
    'pr_projstatus_good': FileContent(),
    'pr_projstatus_fail': PR_ProjStatus_Fail(),
}

def gen_content(action_type: Briareus.Actions.ActionType,
                notification: Notify,
                runctxt) -> Tuple[str, str]:
    """Generates a tagline and content for the specified Notify
       notification.  The action_type can be used to adjust the
       message to be appropriate to the action type.

       The tagline is a short note for this notification, useable as
       an email subject line or a tooltip, etc.

       The content is the fully expanded text (appropriate to the
       action_type) describing the notification in appropriate detail.
    """
    gen = notify_generators.get(notification.what, FileContent())
    if not gen:
        return 'Build Issue', str(notification)
    return gen(action_type, notification, runctxt) # type: ignore
