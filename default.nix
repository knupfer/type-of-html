{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:

nixpkgs.haskell.packages.${compiler}.callCabal2nix "type-of-html" (nixpkgs.lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}

