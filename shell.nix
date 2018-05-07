{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "lodjurEnv";
  buildInputs = [ postgresql100 zlib ];
  inherit ghc;
}
