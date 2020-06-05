{ mkDerivation, base, containers, fetchgit, hedgehog, mtl
, primitive, stdenv, transformers, vector
}:
mkDerivation {
  pname = "disjoint-sets";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/chessai/disjoint-sets";
    sha256 = "0nvc1mki4614n1qmvggm8f6g35ybzbmsj70pz5yag8zi1jgskxi6";
    rev = "de4370476a56991756610ed36a244ad9adcba223";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers mtl primitive transformers vector
  ];
  testHaskellDepends = [ base hedgehog mtl transformers ];
  homepage = "https://github.com/chessai/disjoint-sets";
  description = "union find (FAST, GOOD, DOESN't SUCK)";
  license = stdenv.lib.licenses.mit;
}
