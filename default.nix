{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (pkgs) fetchFromGitHub;
  inherit (pkgs.haskellPackages) callCabal2nix;
  optparse-generic = callCabal2nix "optparse-generic" (fetchFromGitHub {
    owner = "Gabriel439";
    repo = "Haskell-Optparse-Generic-Library";
    sha256 = "0xy5n8pkvlfh542sngnz0zy1gdmkjmv223yl0d47b20vnmcgkv0r";
    rev = "334f7068186e1a56914a3d2ec73a1b9c4823f364";
  }) {};
in
  pkgs.haskellPackages.callPackage ./regex-fsm.nix { inherit optparse-generic; }
