import ((import <nixpkgs> {}).fetchFromGitHub {
  owner  = "knupfer";
  repo   = "hs-nix-default";
  rev    = "b0a6d3e534eca0ec37fc153fc53db5680e716c26";
  sha256 = "13spcgj40b9w2gyis5jkbi05vlb5v20vh09h8v5y262j8cw2iykn";})

  { project = "type-of-html";
    directory = ./.;
    versions =
      [
#        { "9.0.1" = "master";}
        "8.10.3"
        "8.8.4"
        { "8.6.5" = "nixpkgs_20_03";}
        { "8.4.4" = "nixpkgs_2018";}
        { "8.2.2" = "nixpkgs_2018";}
      ];
  }
