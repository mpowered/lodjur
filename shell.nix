{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, hashable, http-types, lucid, mtl
      , optparse-applicative, postgresql-simple, process, scotty, stdenv
      , text, time, unordered-containers, uuid
      }:
      mkDerivation {
        pname = "lodjur";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base hashable http-types lucid mtl optparse-applicative
          postgresql-simple process scotty text time unordered-containers
          uuid
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
