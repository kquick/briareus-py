"""This is a utility that allows files to be written but ensures that
they are updated atomically after the write completes (via renaming)
to prevent asynchronous processes from reading a partially-written
file.  In addition, the target file is only updated if it has actually
been changed; if there is no update, nothing is written."""

import os
import tempfile
from typing import Callable, TextIO, Union


__all__ = [ 'FileWriterSession', 'AtomicUpdFileWriter' ]


# Atomic file updates are performed using the OS rename operation
# after the file is written to a different name.  It may be desirable
# to write the updated file to a different location than the
# destination exists, but an OS rename will only work on the same
# volume.

def use_tempdir(tempdir: Union[str,
                               tempfile.TemporaryDirectory] = None) -> tempfile.TemporaryDirectory:
    if isinstance(tempdir, tempfile.TemporaryDirectory):
        return tempdir
    tempdir_name = tempdir or os.path.join(os.getcwd(), '.AUWFWS')
    if not tempdir and not os.path.exists(tempdir_name):
        os.mkdir(tempdir_name)
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

       Path portions of the result files are duplicated underneath the
       temporary base directory, so file collisions will only occur on
       full path references.
    """
    def __init__(self, tempdir=None):
        self.files = []
        self.tempdir = use_tempdir(tempdir)
        self._ended = False

    def add_file(self, fname: str,
                 gen_contents: Callable[[TextIO], int]) -> None:
        newfile = AtomicUpdFileWriter(fname, gen_contents,
                                      tempdir=self.tempdir,
                                      in_session=True)
        newfile.generate_new_tempfile()
        self.files.append(newfile)

    def end_session(self) -> None:
        if not self._ended:
            for each in self.files:
                each.atomic_update()
            self._ended = True


class AtomicUpdFileWriter(object):
    """Used to atomically update the target file, and to make no changes
       (even file metadata changes) if the file hasn't changed.

       Passed the target filepath and the function that will
       output contents given a file-stream argument.  The tempdir
       optional argument can specify where the temporary files are to
       be generated.  The in_session argument is intended to be used
       only by the FileWriterSession object.

       The target filepath may be absolute, or it may be relative to
       the current directory, similar to normal file creation
       operations.  If the target filepath is absolute, the temporary
       storage of that file will still be in the special temporary
       location, but it is possible that the temporary location and
       the resulting target location will be on different filesystems
       (this is also possible even with relative paths, but it is more
       likely to occur when an absolute path is used).

    """
    def __init__(self, fname: str,
                 gen_contents: Callable[[TextIO], int],
                 tempdir: tempfile.TemporaryDirectory = None,
                 in_session: bool = False) -> None:
        self.tempdir = use_tempdir(tempdir)
        self.fname = fname
        # The tempdir might be the same directory where the output
        # goes, so adding a ".new" extension prevents collisions.
        # However, collisions aren't actually checked for... that
        # might be a good thing to add here someday.
        self.temp_fname = os.path.join(
            self.tempdir.name,
            (fname[1:] if fname[0] == '/' else fname) + ".new")
        self.gen_contents = gen_contents
        self._generated = False
        if not in_session:
            self.do_atomic_upd_write()

    def do_atomic_upd_write(self) -> None:
        self.generate_new_tempfile()
        self.atomic_update()

    def generate_new_tempfile(self) -> None:
        os.makedirs(os.path.dirname(self.temp_fname), exist_ok=True)
        with open(self.temp_fname, 'w') as outf:
            self.gen_contents(outf)
        self._generated = True

    def atomic_update(self) -> None:
        assert self._generated
        if os.path.exists(self.fname):
            srcdata = open(self.temp_fname, 'r').read(8192000)
            tgtdata = open(self.fname,      'r').read(8192000)
            if srcdata == tgtdata:
                # No change, so file does not need to be updated
                os.remove(self.temp_fname)
                return
        if os.path.exists(self.temp_fname):
            if not os.path.exists(os.path.dirname(self.fname)):
                os.makedirs(os.path.dirname(self.fname), exist_ok=True)
            os.rename(self.temp_fname, self.fname)
