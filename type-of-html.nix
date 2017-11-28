{ mkDerivation, base, blaze-html, bytestring, criterion
, double-conversion, ghc-prim, hspec, QuickCheck, stdenv, text
}:
mkDerivation {
  pname = "type-of-html";
  version = "1.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring double-conversion ghc-prim text
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  benchmarkHaskellDepends = [
    base blaze-html bytestring criterion QuickCheck text
  ];
  homepage = "https://github.com/knupfer/type-of-html";
  description = "High performance type driven html generation";
  license = stdenv.lib.licenses.bsd3;
}
