"Primary module providing generation of content message from Notify logic output"

import sys
import os.path
import Briareus.Actions

class FileContent(object):
    """Use this module for content which should come from local files.
       The file is searched for with
       "{action_type}notification_type.txt" first (with ".tag" for the
       tagline) and if that doesn't exist, then without the
       "{action_type}".  The contents can refer to the notification as
       {n} for format substitution.

    """
    def __call__(self, action_type, notification, runctxt):
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
    def __call__(self, action_type, notification, runctxt):
        if action_type == 'forge_status':

            project = notification.subject

            proj_resultlist = [ each
                                for each in runctxt.result_sets
                                if project == each.inp_desc.PNAME ]
            if not proj_resultlist:
                raise ValueError("Expected to find \"%s\" in: %s" %
                                 (project, [(each.inp_desc.PNAME, r.repo_name)
                                            for each in runctxt.result_sets
                                            for r in each.inp_desc.RL
                                            if r.project_repo]))

            proj_results = proj_resultlist[0]

            return action_type, 'Failing {numfail} of {numtotal} {n.subject} builds.'.format(
                n=notification,
                numfail=len(notification.params.fails),
                numtotal=len(notification.params.fails)+len(notification.params.goods))

        return None, None

notify_generators = {
    'variable_failing': FileContent(),
    'completely_broken': FileContent(),
    'pr_projstatus_pending': FileContent(),
    'pr_projstatus_good': FileContent(),
    'pr_projstatus_fail': PR_ProjStatus_Fail(),
}

def gen_content(action_type, notification, runctxt):
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
    return gen(action_type, notification, runctxt)
