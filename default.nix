{ compiler ? "ghc843" }:

let
  nixpkgs = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-18.09";
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}".override {
    overrides = self: super: {
      lodjur = self.callPackage ./default.nix {};
      hoauth2 = pkgs.haskell.lib.doJailbreak super.hoauth2;
      stm-containers = pkgs.haskell.lib.dontCheck super.stm-containers;
      superbuffer = pkgs.haskell.lib.dontCheck super.superbuffer;
      # stm-containers = hself.callHackage "stm-containers" "1.1.0.2" {};
      # primitive = hself.callHackage "primitive" "0.6.4.0" {};
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
