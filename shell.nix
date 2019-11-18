{ pkgs ? import <nixpkgs> {}
}:
let
  callPackage = pkgs.newScope (pkgs // pkgs.python37Packages);

  briareus = callPackage ./default.nix {};
in
pkgs.lib.overrideDerivation briareus (drv: {
  src = ./.;
  shellHook = ''
    # Not entirely sure why swiProlog isn't added automatically.
    export PATH=${pkgs.swiProlog}/bin:${pkgs.python37Packages.pytest}/bin:$PATH
    # Facilitate running pytest or local hh runs.
    export PYTHONPATH=$(pwd):$PYTHONPATH
  '';
})
