regex-fsm
=======================
The `regex-fsm` tool can be used to convert certain regular expressions to layered finite state machines, which are suitable for use in matrix branching program-based obfuscation tools like the [5GenCrypto](https://github.com/5GenCrypto) obfuscator.

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

To develop `regex-fsm` with `nix`
```
nix-shell -A regex-fsm.env
```

## Usage
```bash
$ regex-fsm --regex '(0|1)' --inputLength 10 --output output.json --verbose True --chunks 1
```

## Construction

### AST

```haskell
Union (Lit '0') (Lit '1')
```

### Epsilon NFA

```haskell
ENFA
  { trans =
      fromList
        [ ( 1 , fromList [ ( Just '0' , fromList [ 2 ] ) ] )
        , ( 3 , fromList [ ( Just '1' , fromList [ 4 ] ) ] )
        , ( 5 , fromList [ ( Nothing , fromList [ 1 , 3 ] ) ] )
        ]
  , start = 5
  , final = fromList [ 2 , 4 ]
  , states = fromList [ 1 , 2 , 3 , 4 , 5 ]
  }
```

### Epsilon Closure

```haskell
fromList
  [ ( 1 , fromList [ 1 ] )
  , ( 2 , fromList [ 2 ] )
  , ( 3 , fromList [ 3 ] )
  , ( 4 , fromList [ 4 ] )
  , ( 5 , fromList [ 1 , 3 , 5 ] )
  ]
```

### DFA

```haskell
DFA
  { trans =
      fromList
        [ ( ( fromList [] , '0' ) , fromList [] )
        , ( ( fromList [] , '1' ) , fromList [] )
        , ( ( fromList [ 1 , 3 , 5 ] , '0' ) , fromList [ 2 ] )
        , ( ( fromList [ 1 , 3 , 5 ] , '1' ) , fromList [ 4 ] )
        , ( ( fromList [ 2 ] , '0' ) , fromList [] )
        , ( ( fromList [ 2 ] , '1' ) , fromList [] )
        , ( ( fromList [ 4 ] , '0' ) , fromList [] )
        , ( ( fromList [ 4 ] , '1' ) , fromList [] )
        ]
  , start = fromList [ 1 , 3 , 5 ]
  , finals = fromList [ fromList [ 2 ] , fromList [ 4 ] ]
  }
```

### Minimized DFA

```haskell
DFA
  { trans =
      fromList
        [ ( ( fromList [] , '0' ) , fromList [] )
        , ( ( fromList [] , '1' ) , fromList [] )
        , ( ( fromList [ 1 , 3 , 5 ] , '0' ) , fromList [ 4 ] )
        , ( ( fromList [ 1 , 3 , 5 ] , '1' ) , fromList [ 4 ] )
        , ( ( fromList [ 4 ] , '0' ) , fromList [] )
        , ( ( fromList [ 4 ] , '1' ) , fromList [] )
        ]
  , start = fromList [ 1 , 3 , 5 ]
  , finals = fromList [ fromList [ 4 ] ]
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
, fromList [ ( "0" , 1 ) , ( "1" , 1 ) ]
]
```

## Tests
To execute the entire pipeline with an arbitrary security parameter
```bash
$ nix-build
$ ./result/bin/obfuscator-tests --secParam 40
```

To run the entire test suite (takes a long time and is very resource intensive)
```bash
$ nix-build
$ ./result/bin/obfuscator-tests --runTestSuites True
```

## LICENSE
[BSD3](License) (c) Galois Inc.
