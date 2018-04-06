{ stdenv }:
stdenv.mkDerivation {
  name = "lodjur-static-files";
  src = ./static;
  buildInputs = [];
  installPhase = "cp -r $src $out";
}
