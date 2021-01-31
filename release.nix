{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8103" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./default.nix { }
