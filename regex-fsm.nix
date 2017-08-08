{ mkDerivation, base, containers, data-default, fgl, fgl-visualize
, hspec, mtl, optparse-generic, parsec, QuickCheck
, quickcheck-instances, stdenv, pretty-show
}:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-default fgl fgl-visualize mtl optparse-generic
    parsec pretty-show
  ];
  testHaskellDepends = [
    base containers fgl hspec QuickCheck quickcheck-instances
  ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to finite state machines";
  license = stdenv.lib.licenses.bsd3;
}
