FROM haskell:9.12.4

RUN mkdir /build-cache
RUN mkdir /code

COPY ./ /code

WORKDIR /code
RUN cabal update
RUN apt update && apt install -y pkg-config libpq-dev
RUN cabal --builddir=/build-cache build --dependencies-only
RUN cabal --builddir=/build-cache test

RUN mkdir /ci
WORKDIR /ci

