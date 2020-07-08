{ pkgs ? import <nixpkgs> {}
}:
let
  callPackage = pkgs.newScope (pkgs // pkgs.python37Packages);

  briareus = callPackage ./default.nix {};

  prolog = builtins.head (builtins.filter (d: d.pname == "swi-prolog") briareus.buildInputs);

in
pkgs.lib.overrideDerivation briareus (drv: {
  src = ./.;
  shellHook = ''
    # Not entirely sure why swiProlog isn't added automatically.
    export PATH=${prolog}/bin:${pkgs.python37Packages.pytest}/bin:$PATH
    # Facilitate running pytest or local hh runs.
    export PYTHONPATH=$(pwd):$PYTHONPATH
  '';
})
