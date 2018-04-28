{ nixpkgs ? import ./nixpkgs.nix }:

(nixpkgs.haskell.lib.doBenchmark (import ./default.nix {})).env
