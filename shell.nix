let
  shell = (import ./lodjur.nix).lodjur-shell-with;
  devpkgs = p: [ p.hlint ]; # p.brittany ];
in
  shell devpkgs
