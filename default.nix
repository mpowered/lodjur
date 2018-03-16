{ mkDerivation, aeson, base, hashable, http-types, lucid, mtl
, optparse-applicative, process, scotty, sqlite-simple, stdenv
, text, time, unordered-containers, uuid
}:
mkDerivation {
  pname = "lodjur";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base hashable http-types lucid mtl optparse-applicative
    process scotty sqlite-simple text time unordered-containers uuid
  ];
  license = stdenv.lib.licenses.unfree;
}
