{ compiler ? "ghc844" }:

let
  nixpkgs = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-18.09";
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}".override {
    overrides = self: super: {
      github = pkgs.haskell.lib.doJailbreak (self.callPackage ./github {});
      jwt = self.callPackage ./jwt.nix {};
      hoauth2 = pkgs.haskell.lib.doJailbreak super.hoauth2;
      stm-containers = pkgs.haskell.lib.dontCheck super.stm-containers;
      superbuffer = pkgs.haskell.lib.dontCheck super.superbuffer;
    };
  };

  drv = haskellPackages.callCabal2nix "lodjur" ./. {};

in
{
  lodjur = drv;
  lodjur-shell = haskellPackages.shellFor {
    packages = p: [drv];
  };
}
