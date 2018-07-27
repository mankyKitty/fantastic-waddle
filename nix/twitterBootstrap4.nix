let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    tbstrap-pinned = initialNixpkgs.pkgs.lib.importJSON ./twitterBootstrap4.json;
    twitterBootstrap4 = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "twbs";
      repo = "bootstrap";
      inherit (tbstrap-pinned) rev sha256;
    };
  };
in
  initialNixpkgs.stdenv.mkDerivation {
    name = "twitterBootstrapV4";
    version = "4.1.3";
    src = sources.twitterBootstrap4;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/{css,js}

      echo "Copying Minified Bootstrap CSS"
      mv dist/css/bootstrap.min.css $out/css/
      mv dist/css/bootstrap.min.css.map $out/css/

      echo "Copying Minified Bootstrap JS"
      mv dist/js/bootstrap.min.js $out/js/
      mv dist/js/bootstrap.min.js.map $out/js/
    '';
  }
