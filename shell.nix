{ pkgs ? import <nixpkgs> {}
}:
let
  callPackage = pkgs.newScope (pkgs // pkgs.python37Packages);

  briareus = callPackage ./default.nix {};
in
pkgs.lib.overrideDerivation briareus (drv: {
  src = ./.;
})
