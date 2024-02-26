# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages,
#    if they become needed.

let

  sources = import ./sources.nix;
  # TODO Use the thyme from nixpkgs, which is currently marked as broken.
  inherit (sources) thyme;

  getOverlays = pkg : import "${pkg}/nix/overlays.nix";

  # We can overlay Haskell packages here.
  haskellOverlays =
    []
    ++ getOverlays thyme
    ;

in haskellOverlays ++ [ (import ./overlay.nix) ]
