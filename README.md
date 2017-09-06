regex-fsm
=======================
## Table of Contents
- [Synopsis](#synopsis)
- [Build](#build)
- [Development](#development)
- [Nix build](#Nix-build)
- [Usage](#usage)
- [Construction](#construction)
- [License](#license)

## Synopsis

The `regex-fsm` tool can be used to convert certain regular expressions to layered finite state machines, which are suitable for use in matrix branching program-based obfuscation tools like the [5GenCrypto](https://github.com/5GenCrypto) obfuscator.

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
$ regex-fsm -r '01' -i 3 -o output.json -v True
```

### Construction
```bash
== Regular Expression AST ==
Cat (Lit '0') (Lit '1')
== Thompson's construction ==
ENFA
  { trans =
      fromList
        [ ( 1 , fromList [ ( Just '0' , fromList [ 2 ] ) ] )
        , ( 2 , fromList [ ( Nothing , fromList [ 3 ] ) ] )
        , ( 3 , fromList [ ( Just '1' , fromList [ 4 ] ) ] )
        ]
  , start = 1
  , final = fromList [ 4 ]
  , states = fromList [ 1 , 2 , 3 , 4 ]
  }
== Closure construction ==
fromList
  [ ( 1 , fromList [ 1 ] )
  , ( 2 , fromList [ 2 , 3 ] )
  , ( 3 , fromList [ 3 ] )
  , ( 4 , fromList [ 4 ] )
  ]
== Subset construction ==
DFA
  { trans =
      fromList
        [ ( ( fromList [] , '0' ) , fromList [] )
        , ( ( fromList [] , '1' ) , fromList [] )
        , ( ( fromList [ 1 ] , '0' ) , fromList [ 2 , 3 ] )
        , ( ( fromList [ 1 ] , '1' ) , fromList [] )
        , ( ( fromList [ 2 , 3 ] , '0' ) , fromList [] )
        , ( ( fromList [ 2 , 3 ] , '1' ) , fromList [ 4 ] )
        , ( ( fromList [ 4 ] , '0' ) , fromList [] )
        , ( ( fromList [ 4 ] , '1' ) , fromList [] )
        ]
  , start = fromList [ 1 ]
  , finals = fromList [ fromList [ 4 ] ]
  }
== Minimized DFA ==
DFA
  { trans =
      fromList
        [ ( ( fromList [] , '0' ) , fromList [] )
        , ( ( fromList [] , '1' ) , fromList [] )
        , ( ( fromList [ 1 ] , '0' ) , fromList [ 2 , 3 ] )
        , ( ( fromList [ 1 ] , '1' ) , fromList [] )
        , ( ( fromList [ 2 , 3 ] , '0' ) , fromList [] )
        , ( ( fromList [ 2 , 3 ] , '1' ) , fromList [ 4 ] )
        , ( ( fromList [ 4 ] , '0' ) , fromList [] )
        , ( ( fromList [ 4 ] , '1' ) , fromList [] )
        ]
  , start = fromList [ 1 ]
  , finals = fromList [ fromList [ 4 ] ]
  }
== Matrix Branching Program ==
[ fromList [ ( "0" , 0 1 ) , ( "1" , 1 0 ) ]
, fromList [ ( "0" , (1 0) (1 0) ) , ( "1" , (1 0) (0 1) ) ]
, fromList [ ( "0" , 1 1 ) , ( "1" , 1 1 ) ]
]
```

## LICENSE
[BSD3](License) (c) Galois Inc.
