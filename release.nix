{ nixpkgs ? import <nixpkgs> {} }:
let
  eval = ghc: nixpkgs.haskell.lib.buildStrictly (import ./default.nix { compiler = ghc; });
in rec {

     ghc802  = eval "ghc802";
     ghc821  = eval "ghc821";
     ghcHEAD = eval "ghcHEAD";

     sdist = nixpkgs.haskell.lib.sdistTarball ghc821;

}
