let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs { config.allowBroken = true; };
  beamPkg = x: haskellPackages.callCabal2nix x "${sources.beam}/${x}";
  spockPkg = x: haskellPackages.callCabal2nix x "${sources.Spock}/${x}";

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell.lib) doJailbreak dontCheck;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      beam-core = beamPkg "beam-core" {};
      beam-postgres = dontCheck (beamPkg "beam-postgres" {});
      beam-migrate = beamPkg "beam-migrate" {};
      Spock = spockPkg "Spock" {};
      Spock-core = spockPkg "Spock-core" {};
      github = pkgs.haskell.lib.doJailbreak (
               pkgs.haskell.lib.dontHaddock (
                 self.callPackage ./github {}
               ));
      jwt = self.callPackage ./jwt.nix {};
      superbuffer = dontCheck super.superbuffer;
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
