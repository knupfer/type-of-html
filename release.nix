{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.haskell.lib;
with nixpkgs.lib;
let tested = [ "ghc822" "ghc844" "ghc862" ];
    eval = x: import ./default.nix { nixpkgs = nixpkgs; compiler = x; };
in
{ sdist = sdistTarball (eval (last tested));
} // genAttrs tested eval
