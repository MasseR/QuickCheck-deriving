{ mkDerivation, base, QuickCheck, stdenv, text, time }:
mkDerivation {
  pname = "QuickCheck-deriving";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base QuickCheck text time ];
  testHaskellDepends = [ base QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
