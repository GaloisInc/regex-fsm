{ mkDerivation, aeson, base, boltzmann-samplers, bytestring
, containers, criterion, data-default, deepseq, directory, hspec
, matrix, mtl, optparse-generic, parsec, pretty-show, process
, QuickCheck, quickcheck-instances, random, split, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base boltzmann-samplers bytestring containers criterion
    data-default deepseq directory hspec matrix mtl optparse-generic
    parsec pretty-show process QuickCheck quickcheck-instances random
    split text unordered-containers
  ];
  testHaskellDepends = [
    aeson base boltzmann-samplers bytestring containers criterion
    data-default deepseq directory hspec matrix mtl parsec pretty-show
    process QuickCheck quickcheck-instances split text
    unordered-containers
  ];
  benchmarkHaskellDepends = [
    aeson base containers criterion deepseq matrix mtl parsec text
    unordered-containers
  ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to finite state machines";
  license = stdenv.lib.licenses.bsd3;
}
