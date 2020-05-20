{ mkDerivation, base, fetchgit, hedgehog, hedgehog-classes
, nonempty-vector, semigroupoids, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "vector-circular";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/chessai/vector-circular";
    sha256 = "0mjpkr7q7zggbviygswxwmzd56vzhlivf127nl2b0dg958jw3q56";
    rev = "296bb22c27e7c75084df9f6f5fb44d1a2b7569c0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base nonempty-vector semigroupoids template-haskell vector
  ];
  testHaskellDepends = [ base hedgehog hedgehog-classes ];
  license = stdenv.lib.licenses.mit;
}
