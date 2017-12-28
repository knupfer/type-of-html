{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

nixpkgs.haskell.lib.overrideSrc
  (nixpkgs.haskell.packages.${compiler}.callPackage ./type-of-html.nix {})
  {src = nixpkgs.lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"];}
