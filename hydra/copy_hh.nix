{ nixpkgs, hh_output }:
let pkgs = import <nixpkgs> {}; in
{ jobsets = pkgs.stdenv.mkDerivation {
    name = "copy_hh";
    phases = [ "installPhase" ];
    installPhase = "cp ${hh_output} $out";
  };
}
