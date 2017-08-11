{ mkDerivation, base, boltzmann-samplers, containers, data-default
, fgl, fgl-visualize, hspec, mtl, optparse-generic, parsec
, pretty-show, QuickCheck, quickcheck-instances, stdenv
}:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base boltzmann-samplers containers data-default fgl fgl-visualize
    mtl optparse-generic parsec pretty-show
  ];
  testHaskellDepends = [
    base containers data-default fgl hspec mtl parsec pretty-show
    QuickCheck quickcheck-instances
  ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to finite state machines";
  license = stdenv.lib.licenses.bsd3;
}
