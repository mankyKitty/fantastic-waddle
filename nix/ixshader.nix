let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    ixshader-pinned = initialNixpkgs.pkgs.lib.importJSON ./ixshader.json;
    ixshader = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "schell";
      repo = "ixshader";
      inherit (ixshader-pinned) rev sha256;
    };
  };
in
  sources.ixshader
