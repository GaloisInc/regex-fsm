FROM nixos/nix:1.11
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-env -iA gitMinimal -f '<nixpkgs>'
RUN git clone https://github.com/GaloisInc/regex-fsm
WORKDIR regex-fsm
RUN nix-build
CMD ./result/bin/obfuscator-tests
