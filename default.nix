{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
nixpkgs.haskell.lib.buildStrictly (nixpkgs.haskell.packages.${compiler}.callPackage ./type-of-html.nix {})
