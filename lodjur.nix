let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs { config.allowBroken = true; };
  beamPkg = x: haskellPackages.callCabal2nix x "${sources.beam}/${x}";

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell.lib) doJailbreak dontCheck dontHaddock;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      beam-core = beamPkg "beam-core" {};
      beam-postgres = dontCheck (beamPkg "beam-postgres" {});
      beam-migrate = beamPkg "beam-migrate" {};
      github = doJailbreak ( dontHaddock ( self.callPackage ./github {} ) );
      jwt = self.callPackage ./jwt.nix {};
      superbuffer = dontCheck super.superbuffer;

      # Not on hackage yet
      servant-event-stream = (import sources.servant-event-stream { inherit pkgs; }).servant-event-stream;

      # For shell: bittany deps
      multistate = dontCheck super.multistate;
      brittany = doJailbreak super.brittany;
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
    withHoogle = true;
    shellHook = ''
        export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
      '';
  };
}
