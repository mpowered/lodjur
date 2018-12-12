{ compiler ? "ghc844" }:
(import ./. { inherit compiler; }).lodjur-shell
