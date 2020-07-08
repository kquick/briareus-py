{ stdenv, fetchFromGitHub, python3Packages
# , swiProlog
, thespian, setproctitle, attrs, requests
, openssl_1_1, pkgs
}:

let
  # prolog = swiProlog.overrideAttrs (oldAttrs: {
  #   openssl = openssl_1_1;
  # });
  # The above doesn't remove dependency on openssl_1_0_2, so use the following:

  prolog = let l = <nixpkgs/pkgs/development/compilers/swi-prolog>;
           in pkgs.callPackage l {
             openssl = openssl_1_1;
             Security = null;
           };

in
python3Packages.buildPythonApplication rec {
  pname = "briareus";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "kquick";
    repo = "briareus";
    rev = "v0.2";
    sha256 = "1hd8xbwfxz1ngxrpqfsyc2jk516d952m15zs3pzf02i8ny8k5s6q";
  };
  # src = python3Packages.fetchPypi {
  #   inherit pname version;
  #   sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  # };

  propagatedBuildInputs = [
    thespian
    setproctitle
    attrs
    requests
  ];

  buildInputs = [
    prolog
  ];

  checkInputs = propagatedBuildInputs;
  doCheck = false;

  meta = {
    description = "Build configuration generator";
    maintainers = [ stdenv.lib.maintainers.kquick ];
    # license = stdenv.lib.licenses.?;
    # homepage = "?";
  };
}
