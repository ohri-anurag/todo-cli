FROM ghcr.io/ohri-anurag/ci-generator:latest

RUN mkdir /tmp
RUN mkdir /build-cache
RUN mkdir /code

COPY ./ /code

WORKDIR /code
RUN cabal update
RUN cabal --builddir=/build-cache build --dependencies-only
RUN cabal --builddir=/build-cache test

RUN mkdir /ci
WORKDIR /ci

