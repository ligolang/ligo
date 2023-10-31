FROM alpine:3.18 as ligo-builder

WORKDIR /ligo

ADD https://github.com/ocaml/opam/releases/download/2.1.0/opam-2.1.0-x86_64-linux /usr/local/bin/opam

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig findutils rsync \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev \
  cargo py3-pip cmake  \
  && pip3 install jsonschema \
  # install opam:
  # not using install_opam.sh because it does `opam init` with `-a` and not `--disable-sandboxing`
  # not using official opam installer because it requires user input
  && chmod u+x /usr/local/bin/opam \
  && opam init --disable-sandboxing --bare

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
COPY dune dune-project ligo_unix.ml /ligo/
COPY configurator /ligo/configurator
COPY src /ligo/src
COPY scripts/version.sh /ligo/scripts/version.sh

COPY tools/ligo-syntax-highlighting ligo-syntax-highlighting

# Run tests
RUN opam exec -- dune runtest --profile static --no-buffer \
# Coverage (only the overall)
  && find . -name '*.coverage' | xargs rm -f \
  && opam exec -- dune clean \
  && mkdir highlighting highlighting/vim highlighting/emacs highlighting/vscode highlighting/textmate \
  # Generate syntax highlighting files
  && opam exec -- dune exec ligo-syntax-highlighting/LigoSyntaxHighlighting.exe -- --vim=highlighting/vim --emacs=highlighting/emacs --vscode=highlighting/vscode --textmate=highlighting/textmate


COPY changelog.txt /ligo/changelog.txt
ENV CHANGELOG_PATH=/ligo/changelog.txt

# Version info and changelog
##################
# Code between TAG_REMOVE_IN_CASE_OF_MR is used for the ligo changelog command. It's useful 
# to trace the used version in case of debugging or usage of next tag. But it'll create divergence on cache for each commit.
# So we remove it to optimize CI in case of MR.
# To debug your MR you can build it locally, or remove the line which remove lines in CI
##################
### TAG_REMOVE_IN_CASE_OF_MR ###
ARG ligo_version
### TAG_REMOVE_IN_CASE_OF_MR ###

RUN LIGO_VERSION=$ligo_version opam exec -- dune build -p ligo --profile static \
  # Copy binary now to avoid problems with BISECT_ENABLE below
  && cp /ligo/_build/install/default/bin/ligo /tmp/ligo \
  # Run doc
  && opam exec -- dune build @doc

FROM esydev/esy:nightly-alpine as esy

# TODO see also ligo-docker-large in nix build
FROM alpine:3.18 as ligo
# This variable is used for analytics to determine if th execution of the compiler is inside docker or not
ENV DOCKER_EXECUTION=true

COPY --from=esy . .

WORKDIR /root/
RUN chmod 755 /root # so non-root users inside container can see and execute /root/ligo
COPY --from=0 /tmp/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/highlighting /root/highlighting
ENTRYPOINT ["/root/ligo"]

FROM ligo as ligo-ci
# This variable is used for analytics to determine if th execution of the compiler is inside docker or not
ENV LIGO_SKIP_ANALYTICS=true
