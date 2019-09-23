{ stdenv }:

stdenv.mkDerivation {
  name = "briareus_director";
  phases = [ "installPhase" ];
  installPhase = "cp * $out";
}
