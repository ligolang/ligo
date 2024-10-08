FROM alpine:3.20 as ligo-builder

WORKDIR /ligo

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig findutils rsync \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev \
  cargo py3-jsonschema cmake opam

RUN opam init --disable-sandboxing --bare

# make bls12-381 build ???
ENV RUSTFLAGS='--codegen target-feature=-crt-static'
# Make sure BLST_PORTABLE is used to build tezos sub-module
# If this flag is not setup, old processor can raise an illegal hardware instruction when Tezos emit ADX instructions
ENV ENV BLST_PORTABLE=y

# Install opam switch & deps
COPY scripts/setup_switch.sh /ligo/scripts/setup_switch.sh
RUN opam update \
  && sh scripts/setup_switch.sh
COPY scripts/install_opam_deps.sh /ligo/scripts/install_opam_deps.sh
COPY vendors /ligo/vendors
COPY vendored-dune /ligo/vendored-dune
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo

# install all transitive deps
RUN opam update && sh scripts/install_opam_deps.sh

COPY gitlab-pages /ligo/gitlab-pages
# Install LIGO
COPY dune dune-project /ligo/
COPY configurator /ligo/configurator
COPY src /ligo/src
COPY lib /ligo/lib
COPY scripts/version.sh /ligo/scripts/version.sh

COPY tools/ligo-syntax-highlighting ligo-syntax-highlighting

# Version info
##################
# Code between TAG_REMOVE_IN_CASE_OF_MR is used for the ligo changelog command. It's useful 
# to trace the used version in case of debugging or usage of next tag. But it'll create divergence on cache for each commit.
# So we remove it to optimize CI in case of MR.
# To debug your MR you can build it locally, or remove the line which remove lines in CI
##################
### TAG_REMOVE_IN_CASE_OF_MR ###
ARG ligo_version
### TAG_REMOVE_IN_CASE_OF_MR ###

# Tests
##################
# Code between TAG_REMOVE_IN_CASE_OF_SKIPTEST will be removed in case of skip test.
# Useful if the package is build but have already been tested
##################
### TAG_REMOVE_IN_CASE_OF_SKIPTEST ###
# Run tests
RUN opam exec -- dune runtest -j 5 --profile static --no-buffer \
# Coverage (only the overall)
  && find . -name '*.coverage' | xargs rm -f \
  && opam exec -- dune clean
### TAG_REMOVE_IN_CASE_OF_SKIPTEST ###

# Generate syntax highlighting files
RUN mkdir highlighting highlighting/vim highlighting/emacs highlighting/vscode highlighting/textmate \
  && opam exec -- dune exec ligo-syntax-highlighting/LigoSyntaxHighlighting.exe -- --vim=highlighting/vim --emacs=highlighting/emacs --vscode=highlighting/vscode --textmate=highlighting/textmate

COPY changelog.txt /ligo/changelog.txt
ENV CHANGELOG_PATH=/ligo/changelog.txt


RUN LIGO_VERSION=$ligo_version opam exec -- dune build -p ligo --profile static \
  # Copy binary now to avoid problems with BISECT_ENABLE below
  && cp /ligo/_build/install/default/bin/ligo /tmp/ligo \
  # Run doc
  && opam exec -- dune build @doc

# TODO see also ligo-docker-large in nix build
FROM alpine:3.18 as ligo
# This variable is used for analytics to determine if th execution of the compiler is inside docker or not
ENV DOCKER_EXECUTION=true

WORKDIR /root/
RUN chmod 755 /root # so non-root users inside container can see and execute /root/ligo
COPY --from=0 /tmp/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/highlighting /root/highlighting
ENTRYPOINT ["/root/ligo"]

FROM ligo as ligo-ci
# This variable is used for analytics to determine if th execution of the compiler is inside docker or not
ENV LIGO_SKIP_ANALYTICS=true
