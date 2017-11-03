let
  eval = import ./default.nix;
in rec {

     ghc802  = eval { compiler = "ghc802"; };
     ghc821  = eval { compiler = "ghc821"; };
     ghcHEAD = eval { compiler = "ghcHEAD"; };

}
