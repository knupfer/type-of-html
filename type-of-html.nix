{ mkDerivation, base, blaze-html, bytestring, criterion, hspec
, stdenv, text
}:
mkDerivation {
  pname = "type-of-html";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring text ];
  testHaskellDepends = [ base hspec ];
  benchmarkHaskellDepends = [
    base blaze-html bytestring criterion text
  ];
  description = "High performance type driven html generation";
  license = stdenv.lib.licenses.bsd3;
}
