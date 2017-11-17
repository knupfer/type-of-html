{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./type-of-html.nix {}
