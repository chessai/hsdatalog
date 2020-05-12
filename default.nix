{ pkgs ? import ./.nix/nixpkgs.nix { inherit compiler; }
, compiler ? "ghc883"
}:

let
  hsPkgs = pkgs.haskellPackages;

  hsdatalog = hsPkgs.callCabal2nix "hsdatalog" ./. {};

in rec {
  inherit pkgs hsPkgs;

  inherit hsdatalog;
}
