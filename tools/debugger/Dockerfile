FROM alpine:3.17.3 as debugger-build-base
# Install GHC 9.0.2 using ghcup
RUN apk update \
  && apk add \
       autoconf automake bash binutils-gold curl dpkg fakeroot file \
       findutils g++ gcc git make perl shadow tar xz \
  && apk add \
       brotli brotli-static \
       bzip2 bzip2-dev bzip2-static \
       curl libcurl curl-static \
       freetype freetype-dev freetype-static \
       gmp-dev \
       libffi libffi-dev \
       libpng libpng-static \
       ncurses-dev ncurses-static \
       openssl-dev openssl-libs-static \
       pcre pcre-dev \
       pcre2 pcre2-dev \
       sdl2 sdl2-dev \
       sdl2_image sdl2_image-dev \
       sdl2_mixer sdl2_mixer-dev \
       sdl2_ttf sdl2_ttf-dev \
       xz xz-dev \
       zlib zlib-dev zlib-static \
  && ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6
ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
RUN curl --fail --output /bin/ghcup \
      'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
 && chmod 0755 /bin/ghcup \
 && ghcup upgrade --target /bin/ghcup \
 && ghcup install cabal --set \
 && /usr/local/.ghcup/bin/cabal update
ENV PATH="/usr/local/.ghcup/bin:$PATH"
RUN ghcup install ghc 9.0.2 --set
# Install stack
RUN wget -qO- https://get.haskellstack.org/ | sh

FROM debugger-build-base as debugger-build
# Install native debugger deps
RUN apk update && apk add libsodium-static libsodium-dev zlib-static zlib-dev pkgconfig
# Prebuild all deps without sources for better caching
RUN mkdir /ligo-debugger
WORKDIR /ligo-debugger
COPY ligo-debugger/stack.yaml /ligo-debugger
COPY ligo-debugger/package.yaml /ligo-debugger
COPY ligo-debugger/stack.yaml.lock /ligo-debugger
COPY ligo-debugger/ligo-debugger.cabal /ligo-debugger
RUN stack build --system-ghc --ghc-options '-Werror -optl=-static -optl=-pthread' --test --no-run-tests --only-dependencies
# Actually build the debugger
COPY ligo-debugger .
# gcc-wrapper.sh is a workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/20168
RUN stack build --system-ghc --ghc-options '-Werror -optl=-static -optl=-pthread -pgml=/ligo-debugger/gcc-wrapper.sh' --copy-bins --local-bin-path="$PWD" --test --no-run-tests
