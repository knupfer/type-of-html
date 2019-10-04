import ((import <nixpkgs> {}).fetchFromGitHub {
  owner  = "knupfer";
  repo   = "hs-nix-default";
  rev    = "3aef8297cb20ff9d0e4d80a70cb9c36b4466d9f2";
  sha256 = "17ssqk5f18fprfq2wy5cv1cd68ycyq12rfnmjg3ryyg1fzdndgaq";})

  { project = "type-of-html";
    directory = ./.;
    versions =
      [
        "8.6.5"
        "8.6.4"
        { "8.4.4" = "nixpkgs_2018";}
        "8.4.3"
        { "8.2.2" = "nixpkgs_2018";}
        "8.2.1"
      ];
  }
