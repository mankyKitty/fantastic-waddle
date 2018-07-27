let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-dom-svg-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-dom-svg.json;
    reflex-dom-svg = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-svg";
      inherit (reflex-dom-svg-pinned) rev sha256;
    };
  };
in
  sources.reflex-dom-svg
