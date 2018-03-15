#!/bin/sh

nix-env -f release.nix -iA lodjur -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/c80adf5f72b8ea4ebd83d04bfbfbdc4cebbeed6f.tar.gz
