regex-fsm
=======================

# Synopsis

The `regex-fsm` tool can be used to convert certain regular expressions to layered finite state machines, which are suitable for use in matrix branching program-based obfuscation tools

## Installation

## Build

```bash
cabal sandbox init
cabal install -j --depedencies-only
```

## Development

```
cabal build
```

## Building with `nix`

To build
```
nix-build
```

To develop `regex-fsm`
```
nix-shell -A regex-fsm.env
```
