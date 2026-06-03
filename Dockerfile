FROM haskell:9.12.4-slim-bookworm

RUN mkdir /build-cache /code /code/src /code/test /code/exe

COPY ./cabal.project.freeze /code
COPY ./CHANGELOG.md /code
COPY ./LICENSE /code
COPY ./todo-cli.cabal /code
COPY ./src /code/src
COPY ./test /code/test
COPY ./exe /code/exe

WORKDIR /code
RUN apt update && apt install -y pkg-config libpq-dev
RUN cabal update \
   && cabal --builddir=/build-cache build --dependencies-only --disable-documentation --disable-library-profiling --disable-benchmarks --disable-tests \
   && cabal --builddir=/build-cache test \
   && cabal clean && rm -rf /root/.cache/cabal/packages/hackage.haskell.org/01-index.tar /root/.cache/cabal/logs

RUN mkdir /ci
WORKDIR /ci

