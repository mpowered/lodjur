let
  shell = (import ./. {}).lodjur-shell-with;
  devpkgs = p: [ p.brittany ];
in
  shell devpkgs
