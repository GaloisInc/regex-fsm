{ mkDerivation, aeson, base, bytestring, containers, criterion
, data-default, deepseq, directory, hspec, matrix, mtl
, optparse-generic, parsec, pretty-show, process
, quickcheck-instances, random, split, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers criterion data-default deepseq
    directory hspec matrix mtl optparse-generic parsec pretty-show
    process quickcheck-instances random split text unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers criterion data-default deepseq
    directory hspec matrix mtl parsec pretty-show process
    quickcheck-instances split text unordered-containers
  ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to matrix branching programs";
  license = stdenv.lib.licenses.bsd3;
}
