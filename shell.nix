{ nixpkgs ? import <nixpkgs> {}
}:
nixpkgs.callPackage ./default.nix {}
