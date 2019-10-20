{ mkDerivation, aeson, base, bytestring, dhall, hspec, http-client
, http-types, HUnit, keys, lens, monad-logger, mtl, prettyprinter
, stdenv, text, transformers, unordered-containers, wreq
}:
mkDerivation {
  pname = "aws-lambda-haskell-runtime-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring dhall http-client http-types lens
    monad-logger mtl prettyprinter text transformers
    unordered-containers wreq
  ];
  executableHaskellDepends = [
    aeson base bytestring dhall http-client http-types lens
    monad-logger mtl text transformers unordered-containers wreq
  ];
  testHaskellDepends = [
    aeson base bytestring hspec http-client http-types HUnit keys lens
    monad-logger mtl text transformers unordered-containers wreq
  ];
  homepage = "https://github.com/EarnestResearch/aws-lambda-haskell-runtime-client#readme";
  description = "AWS Lambda Haskell Runtime Client";
  license = stdenv.lib.licenses.mit;
}
