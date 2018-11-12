{ nixpkgs ? import ./nixpkgs.nix, compiler ? "" }:

with nixpkgs;
let ghc = if compiler == "" then haskellPackages else haskell.packages.${compiler}; in
haskell.lib.shellAware (haskell.lib.doBenchmark (ghc.callCabal2nix "type-of-html" (lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}))


