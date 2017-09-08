{ mkDerivation, aeson, miso, stdenv }:
mkDerivation {
  pname = "regex-fsm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson miso ];
  homepage = "https://github.com/GaloisInc/regex-fsm";
  description = "Convert regular expressions to matrix branching programs";
  license = stdenv.lib.licenses.bsd3;
}
