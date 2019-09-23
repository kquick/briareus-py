{ lib, python3Packages, swiProlog }:

python3Packages.buildPythonApplication rec {
  pname = "briareus";
  version = "0.1";

  src = ./.;
  # src = python3Packages.fetchPypi {
  #   inherit pname version;
  #   sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  # };

  propagatedBuildInputs =
    let ppkgs = with python3Packages; [
          thespian
          setproctitle
          attrs
          requests
        ];
    in [swiProlog] ++ ppkgs;

  meta = {
    description = "Build configuration generator";
    maintainers = [ lib.maintainers.kquick ];
    # license = lib.licenses.?;
    # homepage = "?";
  };
}
