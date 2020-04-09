"""This is a utility that allows files to be written but ensures that
they are updated atomically after the write completes (via renaming)
to prevent asynchronous processes from reading a partially-written
file.  In addition, the target file is only updated if it has actually
been changed; if there is no update, nothing is written."""

import os
import tempfile

__all__ = [ 'FileWriterSession', 'AtomicUpdFileWriter' ]


# Atomic file updates are performed using the OS rename operation
# after the file is written to a different name.  It may be desirable
# to write the updated file to a different location than the
# destination exists, but an OS rename will only work on the same
# volume.

def use_tempdir(tempdir=None):
    if isinstance(tempdir, tempfile.TemporaryDirectory):
        return tempdir
    tempdir_name = tempdir or os.path.join(os.getcwd(), '.AUWFWS')
    if not tempdir and not os.path.exists(self.tempdir_name):
        os.mkdir(self.tempdir_name)
    if not os.path.exists(tempdir_name):
        raise RuntimeError('AtomicUpdWriter invalid tempdir: %s'
                           % tempdir_name)
    return tempfile.TemporaryDirectory(dir=tempdir_name)


class FileWriterSession(object):
    """Controls a session where one or more files may be atomically
       updated.  Uses the AtomicUpdFileWriter for each file, but
       synchronizes the updates to be performed all at once at the end
       of the session.

       Note that the individual files are updated atomically, but
       there is still a small window where an observer would see some
       files updated and others not updated.  This window is very
       small however.  The primary benefit of using the
       FileWriterSession is that the files are all updated as closely
       as possible instead of spreading the updates out over a broader
       period of time.

       Also note that a single temporary location is used for all
       updated files, irrespective of any path element of the ultimate
       target.  This means that if any of the files share the same
       filename that there will be a collision/overwrite.

    """
    def __init__(self, tempdir=None):
        self.files = []
        self.tempdir = use_tempdir(tempdir)
        self._ended = False

    def add_file(self, fname, gen_contents):
        newfile = AtomicUpdFileWriter(fname, gen_contents,
                                      tempdir=self.tempdir,
                                      in_session=True)
        newfile.generate_new_tempfile()
        self.files.append(newfile)

    def end_session(self):
        if not self._ended:
            for each in self.files:
                each.atomic_update()
            self._ended = True


class AtomicUpdFileWriter(object):
    """Used to atomically update the target file, and to make no changes
       (even file metadata changes) if the file hasn't changed.

       Passed the target filename and the function that will output
       contents given a file-stream argument.  The tempdir optional
       argument can specify where the temporary files are to be
       generated.  The in_session argument is intended to be used only
       by the FileWriterSession object.
    """
    def __init__(self, fname, gen_contents, tempdir=None, in_session=False):
        self.tempdir = use_tempdir(tempdir)
        self.fname = fname
        # The tempdir might be the same directory where the output
        # goes, so adding a ".new" extension prevents collisions.
        # However, collisions aren't actually checked for... that
        # might be a good thing to add here someday.
        self.temp_fname = os.path.join(self.tempdir.name,
                                       os.path.basename(self.fname) + ".new")
        self.gen_contents = gen_contents
        self._generated = False
        if not in_session:
            self.do_atomic_upd_write()

    def do_atomic_upd_write(self):
        self.generate_new_tempfile()
        self.atomic_update()

    def generate_new_tempfile(self):
        with open(self.temp_fname, 'w') as outf:
            self.gen_contents(outf)
        self._generated = True

    def atomic_update(self):
        assert self._generated
        if os.path.exists(self.fname):
            srcdata = open(self.temp_fname, 'r').read(8192000)
            tgtdata = open(self.fname,      'r').read(8192000)
            if srcdata == tgtdata:
                # No change, so file does not need to be updated
                os.remove(self.temp_fname)
                return
        if os.path.exists(self.temp_fname):
            os.rename(self.temp_fname, self.fname)
