FROM ghcr.io/ohri-anurag/ci-generator:latest

RUN mkdir /tmp

COPY ./ /

RUN cabal update
RUN cabal build --dependencies-only && cabal test --dependencies-only


