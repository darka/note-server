FROM nixos/nix

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN nix-env -i cabal2nix

COPY . /app
WORKDIR /app
RUN cabal2nix .
RUN nix-build release.nix
ENTRYPOINT result/bin/note-server
