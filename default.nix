import ((import <nixpkgs> {}).fetchFromGitHub {
  owner  = "knupfer";
  repo   = "hs-nix-default";
  rev    = "3d7569d391e988bfa79346d09fc88df825135525";
  sha256 = "1sjry7bd8yx0w7mh99v0c9623qaxk4ksc2dy47pv9bznajhq6kva";})

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
