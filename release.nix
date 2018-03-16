{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  jobs = rec {
    lodjur =
      { system ? builtins.currentSystem }:

      let
        pkgs = import <nixpkgs> { inherit system; };

        haskellPackages =
          if compiler == "default"
          then pkgs.haskellPackages
          else pkgs.haskell.packages.${compiler};

        variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
      in
        variant (haskellPackages.callPackage ./. {});
    };
in
  jobs
