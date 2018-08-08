let
  ob = import ./default.nix {};

  inherit (ob.obelisk.nixpkgs) pkgs;

  bootstrap4 = import ./nix/twitterBootstrap4.nix;

  staticAssets = pkgs.stdenv.mkDerivation {
    name = "FantasticWaddleCSS";
    version = "0.0.1";
    src = ./static;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/css
      cp -R css/* $out/css/
    '';
  };

  buildFrontend = minFe: pkgs.runCommand "buildFrontend" { buildInputs = [ pkgs.zopfli ]; } ''
    mkdir -p $out
    mkdir -p $out/css

    ln -s ${staticAssets}/css/* $out/css/
    ln -s ${bootstrap4.out}/css/bootstrap.min.css $out/css/bootstrap.min.css
    ln -s ${bootstrap4.out}/css/bootstrap.min.css.map $out/css/bootstrap.min.css.map

    mkdir -p $out/js
    ln -s ${minFe}/all.js $out/js/all.min.js
    ln -s ${minFe}/all.js.map $out/js/all.min.js.map

    cd $out/js
    zopfli -i1000 all.min.js
  '';

  out = {
    # Version 3
    staticAssets = staticAssets;
    minifiedFrontend = buildFrontend (ob.obelisk.compressedJs ob.ghcjs.frontend);
  };
in
  out
