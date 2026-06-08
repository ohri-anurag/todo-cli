FROM alpine:3.22.4

ARG CABAL_VERSION=3.16.1.0
ARG GHC_VERSION=9.12.4

RUN apk add \
   bash \
   gcc \
   libc-dev \
   libpq-dev \
   openssl-dev \
   openssl-libs-static \
   pkgconfig \
   postgresql \
   wget \
   zlib-dev \
   zlib-static

RUN wget https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-x86_64-linux-alpine3_12.tar.xz \
   && tar -xf cabal-install-${CABAL_VERSION}-x86_64-linux-alpine3_12.tar.xz \
   && rm cabal-install-${CABAL_VERSION}-x86_64-linux-alpine3_12.tar.xz \
   && mv cabal bin 

RUN wget https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-alpine3_12-linux-static-int_native.tar.xz \
   && tar -xf ghc-${GHC_VERSION}-x86_64-alpine3_12-linux-static-int_native.tar.xz \
   && rm ghc-${GHC_VERSION}-x86_64-alpine3_12-linux-static-int_native.tar.xz \
   && mv ghc-${GHC_VERSION}-x86_64-unknown-linux/bin/ghc-${GHC_VERSION} bin/ghc \
   && mv ghc-${GHC_VERSION}-x86_64-unknown-linux/bin/ghc-pkg-${GHC_VERSION} bin/ghc-pkg \
   && mv ghc-${GHC_VERSION}-x86_64-unknown-linux/bin/hsc2hs-ghc-${GHC_VERSION} bin/hsc2hs \
   && mv ghc-${GHC_VERSION}-x86_64-unknown-linux/bin/unlit-ghc-${GHC_VERSION} bin/unlit \
   && mv ghc-${GHC_VERSION}-x86_64-unknown-linux/lib/* lib/ \
   && rm -rf ghc-${GHC_VERSION}-x86_64-unknown-linux

   
RUN mkdir /build-cache /code /code/src /code/test /code/exe /ci

COPY ./cabal.project.freeze /code
COPY ./CHANGELOG.md /code
COPY ./LICENSE /code
COPY ./todo-cli.cabal /code
COPY ./src /code/src
COPY ./test /code/test
COPY ./exe /code/exe

WORKDIR /code

RUN cabal update \
   && cabal --builddir=/build-cache build --dependencies-only --disable-documentation --disable-library-profiling --disable-benchmarks --disable-tests \
   && cabal --builddir=/build-cache build spec \
   && cabal clean && rm -rf /root/.cache/cabal/packages/hackage.haskell.org/01-index.tar /root/.cache/cabal/logs

WORKDIR /ci

