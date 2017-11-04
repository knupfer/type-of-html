{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./type-of-html.nix {}
