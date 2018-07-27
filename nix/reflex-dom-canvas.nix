let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-dom-canvas-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-dom-canvas.json;
    reflex-dom-canvas = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-canvas";
      inherit (reflex-dom-canvas-pinned) rev sha256;
    };
  };
in
  sources.reflex-dom-canvas
