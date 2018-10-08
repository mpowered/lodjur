{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, cryptonite, github, hashable, hoauth2, htoml, http-client
, http-client-tls, http-types, iso8601-time, lucid, monad-control
, mtl, optparse-applicative, postgresql-simple, process
, resource-pool, Spock, Spock-lucid, stdenv, text, time
, unordered-containers, uri-bytestring, uuid, wai
, wai-middleware-static
}:
mkDerivation {
  pname = "lodjur";
  version = "0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16-bytestring binary bytestring cryptonite github
    hashable hoauth2 htoml http-client http-client-tls http-types
    iso8601-time lucid monad-control mtl optparse-applicative
    postgresql-simple process resource-pool Spock Spock-lucid text time
    unordered-containers uri-bytestring uuid wai wai-middleware-static
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
