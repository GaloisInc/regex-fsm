{ nixpkgs ? import <nixpkgs> {} }:
let
  pkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a3dec324633727e7c541f6ef40aa0a8e80f66549";
    sha256 = "01r1sws35p4p6bpa20jdgnbijhi4dykj41m1az5q6dia6ilaq1hz";
  }) {};
  inherit (pkgs) fetchFromGitHub;
  inherit (pkgs.haskellPackages) callCabal2nix;
  inherit (pkgs.haskell.lib) addExtraLibrary enableCabalFlag dontCheck;
  optparse-generic = callCabal2nix "optparse-generic" (fetchFromGitHub {
    owner = "Gabriel439";
    repo = "Haskell-Optparse-Generic-Library";
    sha256 = "0xy5n8pkvlfh542sngnz0zy1gdmkjmv223yl0d47b20vnmcgkv0r";
    rev = "334f7068186e1a56914a3d2ec73a1b9c4823f364";
  }) {};
  libaesrand = pkgs.stdenv.mkDerivation {
    name = "libaesrand";
    NIX_LDFLAGS = "-lmpfr -lgmp";
    buildInputs = with pkgs; [ flint gmp autoreconfHook openssl mpfr ];
    src = fetchFromGitHub {
      owner = "5GenCrypto";
      repo = "libaesrand";
      sha256 = "0birgh6b71acfnqps9hysvjbagygv6sphvw44gzjnllvf507k3a9";
      rev = "51ce3643511df40fa0b462d21b30a819d58629ff";
    };
  };
  clt13 = pkgs.stdenv.mkDerivation {
    name = "clt13";
    buildInputs = with pkgs; [ libaesrand autoreconfHook openssl gmp flint mpfr ];
    NIX_LDFLAGS = "-lmpfr -lgmp";
    postPatch = "mkdir -p build/autoconf";
    src = fetchFromGitHub {
      owner = "5GenCrypto";
      repo = "clt13";
      sha256 = "07vkxqjv7zw1gzc8hjlvw43yxsblxcyvm1n3pr42ffwv6l3yjf2q";
      rev = "c20d81bbdcffbb755fff88dd256113ebde55b6a0";
    };
  };
  gghlite-flint = pkgs.stdenv.mkDerivation {
    name = "gghlite-flint";
    buildInputs = with pkgs; [ autoreconfHook libaesrand mpfr flint openssl ];
    NIX_LDFLAGS = "-lmpfr -lgmp";
    src = fetchFromGitHub {
      owner = "5GenCrypto";
      repo = "gghlite-flint";
      sha256 = "0nczz5kmhzdmqfrkpjb2yfzprnckz7dajj3z325s9flv57xbkmr9";
      rev = "6efde93edaccef98133de747438d27d4fc76daa3";
    };
  };
  libmmap = pkgs.stdenv.mkDerivation {
    name = "libmmap";
    buildInputs = with pkgs; [ libaesrand autoreconfHook openssl gmp flint mpfr clt13 flint gghlite-flint ];
    NIX_LDFLAGS = "-laesrand -lmpfr -lgmp -lflint";
    src = fetchFromGitHub {
      owner = "5GenCrypto";
      repo = "libmmap";
      sha256 = "01hyy88jmdc8c4vi2v6z49zk39j6i7xh7lvq8dffxsvllj67ir50";
      rev = "d59fb1677ab0b3d83acee9f2416549f1ab0cb468";
    };
  };
  obfuscator-src = fetchFromGitHub {
    owner = "5GenCrypto";
    repo = "obfuscation";
    sha256 = "1ffrv1xqgvv9h2xnl40mqd7g311man53jbf7075w52njrchjdgp4";
    rev = "af6ff702168cd8875597a98e4326490b746caa62";
  };
  obfuscator-c = pkgs.stdenv.mkDerivation {
    name = "obfuscator-c";
    NIX_LDFLAGS = "-lmpfr -lgmp -lflint -lmmap -laesrand -loz";
    buildInputs =
      with pkgs; [
        autoreconfHook
	      libaesrand
	      clt13
	      libmmap
	      flint
	      gmp
	      mpfr
	      openssl
	      gghlite-flint
     ];
    src = obfuscator-src;
  };
  obfuscator = pkgs.pythonPackages.buildPythonPackage {
    name = "obfuscator";
    NIX_LDFLAGS = "-lmpfr -lgmp -lflint -lmmap -laesrand -loz -lobf";
    propagatedBuildInputs = with pkgs.pythonPackages; [ future numpy ];
    buildInputs = with pkgs; [
	    gghlite-flint
	    libaesrand
	    clt13
	    libmmap
	    flint
	    gmp
	    mpfr
      openssl
      obfuscator-c
	    python27
	    pythonPackages.setuptools
      pythonPackages.future
	    pythonPackages.numpy
    ];
    src = obfuscator-src;
  };
  regex-fsm =
     let
       x = enableCabalFlag (pkgs.haskellPackages.callPackage ./regex-fsm.nix {}) "obfuscator-tests";
     in
       pkgs.lib.overrideDerivation x (drv: {
         buildInputs = drv.buildInputs ++ [ obfuscator ];
       });
in
  pkgs.runCommand "regex-fsm" { inherit regex-fsm obfuscator; } ''
    mkdir -p $out/bin
    ln -s ${obfuscator}/bin/obfuscator $out/bin/obfuscator
    ln -s ${regex-fsm}/bin/regex-fsm $out/bin/regex-fsm
    ln -s ${regex-fsm}/bin/obfuscator-tests $out/bin/obfuscator-tests
    ln -s ${regex-fsm}/bin/benchmarks $out/bin/benchmarks
  ''
