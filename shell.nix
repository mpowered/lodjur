args@{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  lodjur = (import ./release.nix args).lodjur;
in
  lodjur.env
