{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, hashable, http-types, lucid
      , monad-control, mtl, optparse-applicative, postgresql-simple
      , process, resource-pool, scotty, stdenv, text, time
      , unordered-containers, uuid
      }:
      mkDerivation {
        pname = "lodjur";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base hashable http-types lucid monad-control mtl
          optparse-applicative postgresql-simple process resource-pool scotty
          text time unordered-containers uuid
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
