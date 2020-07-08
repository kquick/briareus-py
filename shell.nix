{ pkgs ? import <nixpkgs> {}
}:
let
  callPackage = pkgs.newScope (pkgs // pkgs.python37Packages);

  briareus = callPackage ./default.nix {};

  prolog = builtins.head (builtins.filter (d: d.pname == "swi-prolog") briareus.buildInputs);

  testing = pkgs.python37.withPackages(pp: [pp.pytest pp.pytestcov pp.mypy]);
in
pkgs.lib.overrideDerivation briareus (drv: {
  src = ./.;
  shellHook = ''
    # Not entirely sure why swiProlog isn't added automatically.
    export PATH=${prolog}/bin:${testing}/bin:$PATH
    # Facilitate running pytest or local hh runs.
    export PYTHONPATH=$(pwd):$PYTHONPATH
  '';
})
