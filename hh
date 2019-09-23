#! /usr/bin/env nix-shell
# -*- mode: python; -*-
#! nix-shell -i bash -p git swiProlog "python37.withPackages(pp: with pp; [ thespian setproctitle attrs requests ])"

PYTHONPATH=$(pwd):$PYTHONPATH python3 -m Briareus.hh "${@}"
