import attr
from thespian.actors import *
from thespian.runcommand import Command, RunCommand, CommandResult
import Briareus.BCGen.Description as Local   # only for determining module path
from datetime import timedelta
import tempfile
import os

local_path = os.path.dirname(os.path.abspath(Local.__file__))

PROLOG_TIMEOUT = timedelta(seconds=5)

@attr.s(str=False)
class Fact(object):
    fact = attr.ib()

    # Specific str representation for prolog consumption
    def __str__(self): return self.fact + '.'


@attr.s(str=False)
class DeclareFact(object):
    fact_and_arity = attr.ib()

    # Specific str representation for prolog consumption
    def __str__(self): return ':- dynamic ' + self.fact_and_arity + '.'


def run_logic_analysis(analysis_fname, facts, actor_system=None, verbose=False):
    (ffd, factfile) = tempfile.mkstemp(suffix=".pl")
    try:

        # Write out the facts for prolog ingestion
        writefact = lambda f: os.write(ffd, (f+'\n').encode('utf-8'))
        for f in facts:
            writefact(str(f))
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
                          timeout=PROLOG_TIMEOUT,
            )
            cmdrslt = asys.ask(runner, cmd, PROLOG_TIMEOUT)
            asys.tell(runner, ActorExitRequest())
            if isinstance(cmdrslt, CommandResult):
                return cmdrslt.stdout.strip()
            raise RuntimeError('FAIL: ' + str(cmdrslt))

        finally:
            if not actor_system:
                asys.shutdown()

    finally:
        os.unlink(factfile)
        # print('Factfile', str(factfile))
