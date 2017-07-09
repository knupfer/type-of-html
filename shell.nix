{ nixpkgs ? import <nixpkgs> {} }:
(import ./default.nix {}).env
