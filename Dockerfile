FROM ci-generator:latest

RUN mkdir /tmp

COPY ./ /

RUN cabal update
RUN cabal build --dependencies-only


