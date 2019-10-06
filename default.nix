 import ((import <nixpkgs> {}).fetchFromGitHub {
   owner  = "knupfer";
   repo   = "hs-nix-default";
   rev    = "a0afef1c5a75eb899aae023d02f63db8983bcdcb";
   sha256 = "1bjra3bg7600r1zvqsc5lrs3vx7ymp47dyjh1zwwnmz87v9rh0zx";})

  { project = "type-of-html";
    directory = ./.;
    versions =
      [
        { "8.8.1" = "master";}
        "8.6.5"
        "8.6.4"
        { "8.4.4" = "nixpkgs_2018";}
        "8.4.3"
        { "8.2.2" = "nixpkgs_2018";}
        "8.2.1"
      ];
  }
