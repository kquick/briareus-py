{ stdenv, fetchFromGitHub, python3Packages, swiProlog, thespian, setproctitle, attrs, requests }:

python3Packages.buildPythonApplication rec {
  pname = "briareus";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "kquick";
    repo = "briareus";
    rev = "v0.2";
    sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  };
  # src = python3Packages.fetchPypi {
  #   inherit pname version;
  #   sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  # };

  propagatedBuildInputs =
    let ppkgs = [
          thespian
          setproctitle
          attrs
          requests
        ];
    in [swiProlog] ++ ppkgs;

  meta = {
    description = "Build configuration generator";
    maintainers = [ stdenv.lib.maintainers.kquick ];
    # license = stdenv.lib.licenses.?;
    # homepage = "?";
  };
}
