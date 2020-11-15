FROM haskell:8.8.1
COPY . /app
WORKDIR /app
RUN apt update && apt install -y libpq-dev
RUN cabal update
RUN cabal install
ENTRYPOINT note-server
