{ mkDerivation, base, blaze-html, bytestring, criterion, hspec
, stdenv, text
}:
mkDerivation {
  pname = "type-of-html";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base hspec text ];
  benchmarkHaskellDepends = [
    base blaze-html bytestring criterion text
  ];
  description = "High performance type driven html generation";
  license = stdenv.lib.licenses.bsd3;
}
