{ channel ? "nixos-18.09", compiler ? "ghc844" }:

let
  nixpkgs = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = channel;
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}".override {
    overrides = self: super: {
      github = pkgs.haskell.lib.doJailbreak (
               pkgs.haskell.lib.dontHaddock (
                 self.callPackage ./github {}
               ));
      jwt = self.callPackage ./jwt.nix {};
      hoauth2 = pkgs.haskell.lib.doJailbreak super.hoauth2;
      stm-containers = pkgs.haskell.lib.dontCheck super.stm-containers;
      superbuffer = pkgs.haskell.lib.dontCheck super.superbuffer;
    };
  };

  drv = haskellPackages.callCabal2nix "lodjur" ./. {};

in rec
{
  lodjur = drv;
  lodjur-shell = lodjur-shell-with (p: []);
  lodjur-shell-with = devpkgs: haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = devpkgs haskellPackages;
  };
}
