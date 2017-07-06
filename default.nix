{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
  pkgs.haskell.packages.${compiler}.callCabal2nix "regex-fsm" (./.) {}
