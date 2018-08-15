args@{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:

let
  lodjur = (import ./release.nix args).lodjur;
in
  lodjur.env
