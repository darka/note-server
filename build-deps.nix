{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8103" }:
let
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./default.nix { };
in
  builtins.filter (x: x != null) pkg.buildInputs
