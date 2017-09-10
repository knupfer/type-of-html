{ nixpkgs ? import <nixpkgs> {} }:

(nixpkgs.haskell.lib.overrideCabal (import ./default.nix {}) (drv: { libraryHaskellDepends = drv.libraryHaskellDepends ++ drv.benchmarkHaskellDepends; })).env
