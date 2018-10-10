{ compiler ? "ghc843" }:
(import ./. { inherit compiler; }).lodjur-shell
