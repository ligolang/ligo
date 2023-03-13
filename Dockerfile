FROM alpine:3.16 as ligo-builder

WORKDIR /ligo

ADD https://github.com/ocaml/opam/releases/download/2.1.0/opam-2.1.0-x86_64-linux /usr/local/bin/opam

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig findutils rsync \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev nodejs npm \
  cargo py3-pip \
  && pip3 install jsonschema \
  # install opam:
  # not using install_opam.sh because it does `opam init` with `-a` and not `--disable-sandboxing`
  # not using official opam installer because it requires user input
  && chmod u+x /usr/local/bin/opam \
  && opam init --disable-sandboxing --bare

# make bls12-381 build ???
ENV RUSTFLAGS='--codegen target-feature=-crt-static'

# Install opam switch & deps
COPY scripts/setup_switch.sh /ligo/scripts/setup_switch.sh
RUN opam update \
  && sh scripts/setup_switch.sh
COPY scripts/install_opam_deps.sh /ligo/scripts/install_opam_deps.sh
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo
COPY vendors /ligo/vendors
COPY vendored-dune /ligo/vendored-dune

# install all transitive deps
RUN opam update && sh scripts/install_opam_deps.sh

COPY gitlab-pages /ligo/gitlab-pages
# Install LIGO
COPY dune dune-project ligo_unix.ml /ligo/
COPY src /ligo/src
COPY scripts/version.sh /ligo/scripts/version.sh

COPY tools/ligo-syntax-highlighting ligo-syntax-highlighting

# JSOO
COPY jsoo /ligo/jsoo
COPY Makefile /ligo
COPY npm /ligo/npm
COPY examples /ligo/examples

# Run tests
RUN opam exec -- dune build @check \
  && opam exec -- dune runtest --profile static --no-buffer \
# Coverage (only the overall)
  && find . -name '*.coverage' | xargs rm -f \
  && opam exec -- dune clean \
  && mkdir highlighting highlighting/vim highlighting/emacs highlighting/vscode highlighting/textmate \
  # Generate syntax highlighting files
  && opam exec -- dune exec ligo-syntax-highlighting/LigoSyntaxHighlighting.exe -- --vim=highlighting/vim --emacs=highlighting/emacs --vscode=highlighting/vscode --textmate=highlighting/textmate

# Version info and changelog
ARG ci_commit_tag
ARG ci_commit_sha
ARG ci_commit_timestamp
ENV CI_COMMIT_TAG=$ci_commit_tag
ENV CI_COMMIT_SHA=$ci_commit_sha
ENV CI_COMMIT_TIMESTAMP=$ci_commit_timestamp

COPY changelog.txt /ligo/changelog.txt
ENV CHANGELOG_PATH=/ligo/changelog.txt
RUN LIGO_VERSION=$(/ligo/scripts/version.sh) opam exec -- dune build -p ligo --profile static \
  # Copy binary now to avoid problems with BISECT_ENABLE below
  && cp /ligo/_build/install/default/bin/ligo /tmp/ligo \
  # Run doc
  && opam exec -- dune build @doc
RUN npm i -g webpack-cli
RUN cd /ligo && opam exec -- make build-demo-webide
RUN cd /ligo/npm && rm /ligo/npm/ligolang-*.tgz ; npm i && npm run build && npm pack
RUN cd /ligo/examples/ligojs && npm i && npm run build:webpack

FROM esydev/esy:nightly-alpine as esy

# TODO see also ligo-docker-large in nix build
FROM alpine:3.12
COPY --from=esy . .
WORKDIR /root/
RUN chmod 755 /root # so non-root users inside container can see and execute /root/ligo
COPY --from=0 /tmp/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/highlighting /root/highlighting
ENTRYPOINT ["/root/ligo"]
