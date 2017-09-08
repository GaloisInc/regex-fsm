regex-fsm
=======================
[![Build Status](https://travis-ci.org/GaloisInc/regex-fsm.svg?branch=master)](https://travis-ci.org/GaloisInc/regex-fsm)
[![License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/GaloisInc/regex-fsm/blob/master/LICENSE)

The `regex-fsm` tool can be used to convert certain regular expressions to efficient matrix branching programs. These programs are suitable for use in obfuscation tools like the [5GenCrypto](https://github.com/5GenCrypto) obfuscator.

## Table of Contents
- [Build](#build)
- [Development](#development)
- [Nix build](#Nix-build)
- [Usage](#usage)
- [Construction](#construction)
  - [AST](#ast)
  - [Epsilon NFA](#epsilon-nfa)
  - [Epsilon Closure](#epsilon-closure)
  - [DFA](#dfa)
  - [Minimized DFA](#minimized-dfa)
  - [Matrix Branching Program](#matrix-branching-program)
- [Premultiplication](#premultiplication)
  - [Chunks](#chunks)
  - [Result](#result)
- [Benchmarks](#benchmarks)
- [Test](#tests)
- [License](#license)

## Build

```bash
cabal sandbox init
cabal install -j --depedencies-only
```

## Development

```
cabal build
```

## Nix build

To build with [nix](https://nixos.org/nix/)
```
nix-build
```

To develop `regex-fsm` with `nix` (highly recommended)
```
nix-shell -A regex-fsm.env
```

## Usage
```bash
$ regex-fsm --regex '(0|1)*' --inputLength 10 --output output.json --verbose --chunks 1
```

## Construction

### AST

```haskell
Rep (Union (Lit '0') (Lit '1'))
```

### Epsilon NFA

```haskell
ENFA
  { trans =
      fromList
	[ ( 1 , fromList [ ( Just '0' , fromList [ 2 ] ) ] )
	, ( 2 , fromList [ ( Nothing , fromList [ 5 ] ) ] )
	, ( 3 , fromList [ ( Just '1' , fromList [ 4 ] ) ] )
	, ( 4 , fromList [ ( Nothing , fromList [ 5 ] ) ] )
	, ( 5 , fromList [ ( Nothing , fromList [ 1 , 3 ] ) ] )
	]
  , start = 5
  , final = fromList [ 5 ]
  , states = fromList [ 1 , 2 , 3 , 4 , 5 ]
  }
```

### Epsilon Closure

```haskell
fromList
  [ ( 1 , fromList [ 1 ] )
  , ( 2 , fromList [ 1 , 2 , 3 , 5 ] )
  , ( 3 , fromList [ 3 ] )
  , ( 4 , fromList [ 1 , 3 , 4 , 5 ] )
  , ( 5 , fromList [ 1 , 3 , 5 ] )
  ]
```

### DFA

```haskell
DFA
  { trans =
      fromList
	[ ( ( fromList [ 1 , 2 , 3 , 5 ] , '0' )
	  , fromList [ 1 , 2 , 3 , 5 ]
	  )
	, ( ( fromList [ 1 , 2 , 3 , 5 ] , '1' )
	  , fromList [ 1 , 3 , 4 , 5 ]
	  )
	, ( ( fromList [ 1 , 3 , 4 , 5 ] , '0' )
	  , fromList [ 1 , 2 , 3 , 5 ]
	  )
	, ( ( fromList [ 1 , 3 , 4 , 5 ] , '1' )
	  , fromList [ 1 , 3 , 4 , 5 ]
	  )
	, ( ( fromList [ 1 , 3 , 5 ] , '0' ) , fromList [ 1 , 2 , 3 , 5 ] )
	, ( ( fromList [ 1 , 3 , 5 ] , '1' ) , fromList [ 1 , 3 , 4 , 5 ] )
	]
  , start = fromList [ 1 , 3 , 5 ]
  , finals =
      fromList
	[ fromList [ 1 , 2 , 3 , 5 ]
	, fromList [ 1 , 3 , 4 , 5 ]
	, fromList [ 1 , 3 , 5 ]
	]
  }
```

### Minimized DFA

```haskell
DFA
  { trans =
      fromList
	[ ( ( fromList [ 1 , 3 , 5 ] , '0' ) , fromList [ 1 , 3 , 5 ] )
	, ( ( fromList [ 1 , 3 , 5 ] , '1' ) , fromList [ 1 , 3 , 5 ] )
	]
  , start = fromList [ 1 , 3 , 5 ]
  , finals = fromList [ fromList [ 1 , 3 , 5 ] ]
  }
```

### Matrix Branching Program

```haskell
[ fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
, fromList [ ( "0" , 0 ) , ( "1" , 0 ) ]
]
```

### JSON
Suitable for use with the [5Gen](https://github.com/5GenCrypto) obfuscation tool
```javascript
{
  "steps": [{"0":[[1]],"1":[[1]],"position":"0"},
	    {"0":[[1]],"1":[[1]],"position":"1"},
	    {"0":[[1]],"1":[[1]],"position":"2"},
	    {"0":[[1]],"1":[[1]],"position":"3"},
	    {"0":[[1]],"1":[[1]],"position":"4"},
	    {"0":[[1]],"1":[[1]],"position":"5"},
	    {"0":[[1]],"1":[[1]],"position":"6"},
	    {"0":[[1]],"1":[[1]],"position":"7"},
	    {"0":[[1]],"1":[[1]],"position":"8"},
	    {"0":[[0]],"1":[[0]],"position":"9"}]
 ,"outputs": [["false","true"]]
}
```

## Premultiplication
A further optimization before matrix encoding is premultiplication. This will decrease multilinearity, but can increase time spent encoding if the matrix count increases overall. To premultiply matrices, specify a chunk size that is a multiple of the input size.

### Chunks
```bash
$ regex-fsm --regex '(0|1)*' --inputLength 10 --output output.json --verbose --chunks 2
```

### Result
```haskell
[ fromList [ ( "00" , 1 ) , ( "01" , 1 ) , ( "10" , 1 ) , ( "11" , 1 ) ]
, fromList [ ( "00" , 1 ) , ( "01" , 1 ) , ( "10" , 1 ) , ( "11" , 1 ) ]
, fromList [ ( "00" , 1 ) , ( "01" , 1 ) , ( "10" , 1 ) , ( "11" , 1 ) ]
, fromList [ ( "00" , 1 ) , ( "01" , 1 ) , ( "10" , 1 ) , ( "11" , 1 ) ]
, fromList [ ( "00" , 0 ) , ( "01" , 0 ) , ( "10" , 0 ) , ( "11" , 0 ) ]
]
```

## Benchmarks
To execute [criterion](http://www.serpentine.com/criterion/tutorial.html) benchmarks of various compiler phases.
```bash
$ nix-build
$ ./result/bin/benchmarks -o index.html
$ open index.html
```

## Tests
To run the entire pipeline once against the [5Gen](https://github.com/5GenCrypto) obfuscator tool, execute `obfuscator-tests` with a security parameter (defaults to `40`).
```bash
$ nix-build
$ ./result/bin/obfuscator-tests --secParam 8
```

To run the entire test suite (takes a long time and is very resource intensive).
```bash
$ nix-build
$ ./result/bin/obfuscator-tests --runTestSuites
```

To run unit tests, simply `nix-build`, unit tests are run automatically.
```bash
$ nix-build
...
Linking dist/build/tests/tests ...
running tests
Running 1 test suites...
Test suite tests: RUNNING...
Test suite tests: PASS
Test suite logged to: dist/test/regex-fsm-0.1.0.0-tests.log
1 of 1 test suites (1 of 1 test cases) passed.
```

## LICENSE
[BSD3](LICENSE) (c) Galois Inc.
