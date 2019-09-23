{ stdenv }:

stdenv.mkDerivation {
  name = "briareus_director";
  phases = [ "installPhase" ];
  src = ./.;
  installPhase = ''
    echo :::::::::::::::::::: installing briareus_director in $src to $out
    mkdir $out;
    cp $src/* $out;
    echo :::::::::::::::::::: installed  briareus_director
    '';
}
