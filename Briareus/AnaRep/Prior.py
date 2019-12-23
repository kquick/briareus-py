import os.path
import fcntl
import pprint
import time
from Briareus.Types import *

def get_prior_report(report_fname, with_lock=True):
    """Reads the prior report output and returns the open file descriptor
       along with a list of (Briareus.Types) objects.

       If with_lock is True (the default) then the file is locked for
       exclusive access (and this process will wait for 180 1 second
       intervals---3 minutes---for the lock to be released); this is
       useful to prevent multiple instances of Briareus from
       processing the same file at the same time and generating
       conflicting results (especially wrt. notifications).  The
       with_lock can also be set to a non-zero numeric value
       indicating the maximum number of seconds to wait for the lock
       to be released.

       Note that there is no explicit unlock operation.  The
       expectation is that the new report is written to a separate
       file which is than (atomically) renamed to replace this file
       where the new file is not locked.  The exit of this process
       will automatically release the lock as well.

       If the input file does not exist, it is created and locked, but
       remains empty.

       If the input cannot be processed correctly, the file is still
       open and locked but the data is None instead of a list.

    """
    if os.path.exists(report_fname):
        repf = open(report_fname, 'r')
    else:
        repf = open(report_fname, 'x')
    if with_lock:
        for trynum in range(0, 180 if with_lock is True else with_lock):
            try:
                fcntl.flock(repf.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except IOError as e:
                if e.errno != EAGAIN:
                    raise
                else:
                    time.sleep(1)
    try:
        rep = repf.read()
        if rep:
            return repf, [ eval(l, globals(), {})
                           for l in rep.split('\n\n')
                           if l.strip() ]
    except Exception as e:
        print('Warning: unable to process prior report data:', str(e))
    return repf, None

def write_report_output(reportf, report):
    for each in report:
        pprint.pprint(each, stream=reportf)
        print('', file=reportf)
