{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs.haskell.lib;
with nixpkgs.lib;
let tested = ["ghc802" "ghc822" "ghc841" ];
    eval = x: import ./default.nix { compiler = x; };
in
{ sdist = sdistTarball (buildStrictly (eval (last tested)));
  bench = (x: overrideCabal x (x: { isExecutable = true; buildTarget = "test bench"; configureFlags = "--enable-benchmarks --enable-tests --disable-optimization"; }))
          (doBenchmark (eval (last tested)));
} // genAttrs tested (x: buildStrictly (eval x))
