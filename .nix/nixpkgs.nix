{ compiler }:

with {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "3b039f14f2f9ad6cb56975ed64aadd02800893ef";
  sha256 = "0bn2mqfbc6ss7rsaqzplw70dhk512ldgpr5qqzxlllaa0m2m91xn";

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      cudd = self.callPackage ./cudd {};

      haskellPackages = super.haskell.packages."${compiler}".override {
        overrides = hself: hsuper: with super.haskell.lib; rec {
          cudd = unmarkBroken (hsuper.cudd.override { cudd = self.cudd; });
          vector-circular = hself.callPackage ./vector-circular.nix {};
          hedgehog-classes = doJailbreak (unmarkBroken hsuper.hedgehog-classes);
        };
      };
    };
  };

  overlays = [];
};

import (builtins.fetchTarball {
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  inherit sha256;
}) { inherit config overlays; }
