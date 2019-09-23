{ stdenv }:

stdenv.mkDerivation {
  name = "briareus_director";
  phases = [ "installPhase" ];
  src = ./.;
  installPhase = ''
    mkdir $out;
    cp $src/* $out;
    '';
}
