#! /usr/bin/env bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/qfpl/reflex-dom-svg > reflex-dom-svg.json
nix-prefetch-git https://github.com/qfpl/reflex-dom-canvas > reflex-dom-canvas.json
nix-prefetch-git git@github.com:twbs/bootstap > twitterBootstrap4.json
