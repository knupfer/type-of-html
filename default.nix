import ((import <nixpkgs> {}).fetchFromGitHub {
  owner  = "knupfer";
  repo   = "hs-nix-default";
  rev    = "8970a5e1125985dbac7a5b57b4949a105e7ea759";
  sha256 = "14klz15dwwyx2141q66rix2m40slis1vmhy0ijn571xjwr8xd9jp";})

  { project = "type-of-html";
    directory = ./.;
    versions =
      [
        "8.10.2"
        "8.8.4"
        { "8.6.5" = "nixpkgs_2019";}
        { "8.4.4" = "nixpkgs_2018";}
        { "8.2.2" = "nixpkgs_2018";}
      ];
  }
