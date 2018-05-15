{ nixpkgs ? import ./nixpkgs.nix, compiler ? "ghc842" }:

(nixpkgs.haskell.lib.doBenchmark (import ./default.nix {compiler = compiler;})).env
