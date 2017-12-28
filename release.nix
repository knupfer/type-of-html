{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs.haskell.lib;
let
  eval = ghc: import ./default.nix { compiler = ghc; };
  doBench = drv: overrideCabal drv (drv: { isExecutable = true; buildTarget = "test bench"; configureFlags = "--enable-benchmarks --enable-tests --disable-optimization"; });
in rec {
     ghc802 = buildStrictly (eval "ghc802");
     ghc822 = buildStrictly (eval "ghc822");
     #ghcHEAD = eval "ghcHEAD";

     bench = doBench (doBenchmark (eval "ghc822"));
     sdist = sdistTarball ghc822;
}
