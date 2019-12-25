import attr
from thespian.actors import *
from thespian.runcommand import Command, RunCommand, CommandResult
from datetime import timedelta
import tempfile
import os
import sys

local_path = os.path.dirname(os.path.abspath(sys.modules['Briareus.Logic'].__file__))

PROLOG_TIMEOUT = timedelta(minutes=2,seconds=61)

@attr.s(str=False, frozen=True)
class Fact(object):
    fact = attr.ib()

    # Specific str representation for prolog consumption
    def __str__(self): return self.fact + '.'


@attr.s(str=False, frozen=True)
class DeclareFact(object):
    fact_and_arity = attr.ib()

    # Specific str representation for prolog consumption
    def __str__(self):
        return '\n'.join([':- ' + s + ' ' + self.fact_and_arity + '.'
                          for s in [
                                  # 'dynamic',
                                  'discontiguous',
                          ]])


def run_logic_analysis(analysis_fname, facts, raw_logic='', actor_system=None, verbose=False):
    """Runs the prolog logic specification in analysis_fname (which should
       be either an absolute address or relative to the Briareus.Logic
       directory), passing the specified facts (as an array of Fact or
       DeclareFact objects).  Runs the prolog operation synchronously
       (with the PROLOG_TIMEOUT time limit) and returns the stdout
       generated by the prolog operation as a string.

       The raw_logic argument can be used to pass direct Prolog
       statements.  This is commonly used for the reporting control
       directives.

    """
    (ffd, factfile) = tempfile.mkstemp(suffix=".pl")
    try:

        # Write out the facts for prolog ingestion
        writefact = lambda f: os.write(ffd, (f+'\n').encode('utf-8'))
        for f in facts:
            writefact(str(f))
        writefact(raw_logic)
        os.close(ffd)

        # Run prolog via an actor and return the stdout results as a
        # raw string.  Use the multiprocTCPBase (if not already
        # established) to take advantage of the ThespianWatch
        # capability.
        asys = actor_system or ActorSystem('multiprocTCPBase')
        try:
            runner = asys.createActor(RunCommand)
            cmd = Command(exe='swipl',
                          args = ["-f", factfile, "-l", os.path.join(local_path, analysis_fname)],
                          logger=None if verbose else False,  # disable logging unless verbose
                          logtag="{swipl}",
                          max_bufsize=5*1024*1024,
                          timeout=PROLOG_TIMEOUT,
            )
            cmdrslt = asys.ask(runner, cmd, PROLOG_TIMEOUT)
            asys.tell(runner, ActorExitRequest())
            if isinstance(cmdrslt, CommandResult):
                if cmdrslt:
                    warn = cmdrslt.stderr.strip()
                    if warn:
                        print(warn, file=sys.stderr)
                    try:
                        return cmdrslt.stdout.strip()
                    except AttributeError as ex:
                        print("If cmdrslt.stdout is a tuple, that means the middle of the"
                              " output was elided by thespian runcommand; fix this by"
                              " increasing the max_bufsize argument to Command in the code above!",
                              file=sys.stderr)
                        raise
            raise RuntimeError('FAIL: ' + str(cmdrslt))

        finally:
            if not actor_system:
                asys.shutdown()

    finally:
        os.unlink(factfile)
        # print('Factfile', str(factfile))
