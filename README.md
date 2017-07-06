regex-fsm
=======================

# Synopsis

The `regex-fsm` tool can be used to convert certain regular expressions to layered finite state machines, which are suitable for use in matrix branching program-based obfuscation tools

## Installation

Acquire nix
```bash
curl https://nixos.org/nix/install | sh
```

## Build

`nix-build`

## Development

`nix-shell` from the current working directory will put the user into an environment to develop `regex-fsm` with `cabal`.

