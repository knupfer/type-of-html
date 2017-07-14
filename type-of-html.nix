{ mkDerivation, base, blaze-html, bytestring, criterion, hspec
, stdenv, text
}:
mkDerivation {
  pname = "type-of-html";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring text ];
  executableHaskellDepends = [
    base blaze-html bytestring criterion text
  ];
  testHaskellDepends = [ base hspec ];
  description = "High performance type driven html generation";
  license = stdenv.lib.licenses.bsd3;
}
