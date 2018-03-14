{ mkDerivation, base, http-types, lucid, mtl, optparse-applicative
, process, scotty, stdenv, text, time
}:
mkDerivation {
  pname = "lodjur";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base http-types lucid mtl optparse-applicative process scotty text
    time
  ];
  license = stdenv.lib.licenses.unfree;
}
