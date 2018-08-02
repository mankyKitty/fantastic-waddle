let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    gristle-pinned = initialNixpkgs.pkgs.lib.importJSON ./gristle.json;

    gristle = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "schell";
      repo = "gristle";
      inherit (gristle-pinned) rev sha256;
    };
  };

  runHpack = src: initialNixpkgs.runCommand "hpackin" 
    { buildInputs = [ initialNixpkgs.haskellPackages.hpack ]; } '' 
    mkdir $out
    ln -s ${src}/gristle/* $out
    cd $out
    hpack .
  '';
in
  runHpack sources.gristle
