{ nixpkgs ? import ./nixpkgs.nix, compiler ? "" }:

with nixpkgs;
let ghcOLD = if compiler == "" then haskellPackages else haskell.packages.${compiler};
    ghc = ghcOLD.override { overrides = self: super: { criterion = self.criterion_1_5_2_0; }; };
    in

haskell.lib.shellAware (haskell.lib.doBenchmark (ghc.callCabal2nix "type-of-html" (lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}))
