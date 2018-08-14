#! /usr/bin/env fish
ln -s (nix eval -f ../../nix/twitterBootstrap4.nix --raw outPath)/css/bootstrap.min.css
