{ mkDerivation, aeson, base, base-compat, base16-bytestring, binary
, binary-orphans, bytestring, containers, cryptohash-sha1, deepseq
, deepseq-generics, exceptions, file-embed, hashable, hspec
, hspec-discover, http-client, http-client-tls, http-link-header
, http-types, iso8601-time, mtl, network-uri, semigroups, stdenv
, tagged, text, time, tls, transformers, transformers-compat
, unordered-containers, vector, vector-instances
}:
mkDerivation {
  pname = "github";
  version = "0.21";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base-compat base16-bytestring binary binary-orphans
    bytestring containers cryptohash-sha1 deepseq deepseq-generics
    exceptions hashable http-client http-client-tls http-link-header
    http-types iso8601-time mtl network-uri semigroups tagged text time
    tls transformers transformers-compat unordered-containers vector
    vector-instances
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring file-embed hspec
    unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/phadej/github";
  description = "Access to the GitHub API, v3";
  license = stdenv.lib.licenses.bsd3;
}
