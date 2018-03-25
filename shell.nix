{ nixpkgs ? import <nixpkgs> {} }:

(nixpkgs.haskell.lib.doBenchmark (import ./default.nix {})).env
