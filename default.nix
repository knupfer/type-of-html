{ nixpkgs ? import <nixpkgs> {}}:
nixpkgs.haskellPackages.callPackage ./type-of-html.nix {}
