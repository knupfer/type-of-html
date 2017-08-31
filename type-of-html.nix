{ mkDerivation, base, blaze-html, bytestring, criterion, hspec
, stdenv, text
}:
mkDerivation {
  pname = "type-of-html";
  version = "0.2.1.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring text ];
  testHaskellDepends = [ base hspec text ];
  benchmarkHaskellDepends = [
    base blaze-html bytestring criterion text
  ];
  homepage = "https://github.com/knupfer/type-of-html";
  description = "High performance type driven html generation";
  license = stdenv.lib.licenses.bsd3;
}
