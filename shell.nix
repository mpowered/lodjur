let
  shell = (import ./lodjur.nix {}).lodjur-shell-with;
  devpkgs = p: [ p.brittany ];
in
  shell devpkgs
