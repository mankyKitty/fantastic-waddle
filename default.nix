{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {

  overrides = (self: super: let
    # rfx-canvas = import ./nix/reflex-dom-canvas.nix;
    rfx-canvas = (import ../reflex-dom-canvas {}).src;
    rfx-svg = import ./nix/reflex-dom-svg.nix;
  in {
    ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
    ghcWithPackages = self.ghc.withPackages;

    reflex-dom-svg = self.callCabal2nix "reflex-dom-svg" rfx-svg {};
    reflex-dom-canvas = self.callCabal2nix "reflex-dom-canvas" rfx-canvas {};
  });

  # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  # android.displayName = "Obelisk Minimal Example";
  # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  # ios.bundleName = "Obelisk Minimal Example";
})
