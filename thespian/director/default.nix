{ stdenv }:

stdenv.mkDerivation {
  name = "briareus_director";
  phases = [ "installPhase" ];
  installPhase = ''
    echo :::::::::::::::::::: installing briareus_director in $(pwd) to $out
    ls -lh
    echo :::::::::::::::::::: src is $src
    ls -lh $src
    set -x
    cp * $out;
    echo :::::::::::::::::::: installed  briareus_director
    '';
}
