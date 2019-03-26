let
  shell = (import ./. {}).lodjur-shell;
  devpkgs = p: [ p.brittany ];
in
  shell devpkgs
