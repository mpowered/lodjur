{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base16-bytestring, binary
      , bytestring, cryptonite, hashable, htoml, http-types, iso8601-time
      , lucid, monad-control, mtl, optparse-applicative
      , postgresql-simple, process, resource-pool, scotty, stdenv, text
      , time, unordered-containers, uuid, wai, wai-extra
      , wai-middleware-static
      }:
      mkDerivation {
        pname = "lodjur";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base base16-bytestring binary bytestring cryptonite hashable
          htoml http-types iso8601-time lucid monad-control mtl
          optparse-applicative postgresql-simple process resource-pool scotty
          text time unordered-containers uuid wai wai-extra
          wai-middleware-static
        ];
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
