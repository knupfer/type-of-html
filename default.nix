{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

nixpkgs.haskell.packages.${compiler}.callCabal2nix "type-of-html" (nixpkgs.lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}

