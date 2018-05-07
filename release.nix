{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let
  config = {
    allowUnfree = true;

    packageOverrides = super: {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages."${compiler}".override {
            overrides = self: super: {
              lodjur = self.callPackage ./default.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
{
  lodjur = pkgs.haskell.packages."${compiler}".lodjur;
}
