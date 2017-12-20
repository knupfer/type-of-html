{ nixpkgs ? import <nixpkgs> {} }:
let
  eval = ghc: nixpkgs.haskell.lib.buildStrictly (import ./default.nix { compiler = ghc; });
in rec {

     ghc802  = eval "ghc802";
     ghc822  = eval "ghc822";
     ghcHEAD = eval "ghcHEAD";

     sdist = nixpkgs.haskell.lib.sdistTarball ghc822;

}
