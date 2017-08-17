{ mkDerivation, aeson, base, boltzmann-samplers, containers
, data-default, hspec, matrix, mtl, optparse-generic, parsec
, pretty-show, QuickCheck, quickcheck-instances, stdenv
}:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base boltzmann-samplers containers data-default matrix mtl
    optparse-generic parsec pretty-show
  ];
  testHaskellDepends = [
    base boltzmann-samplers containers data-default hspec matrix mtl
    parsec pretty-show QuickCheck quickcheck-instances
  ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to finite state machines";
  license = stdenv.lib.licenses.bsd3;
}
