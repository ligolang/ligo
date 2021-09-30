FROM alpine:3.12 as ligo-builder

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev \
  opam cargo

RUN opam init --disable-sandboxing --bare

# make bls12-381 build ???
ENV RUSTFLAGS='--codegen target-feature=-crt-static'

# Install opam switch & deps
WORKDIR /ligo
COPY scripts/setup_switch.sh /ligo/scripts/setup_switch.sh
RUN opam update && sh scripts/setup_switch.sh
COPY scripts/install_opam_deps.sh /ligo/scripts/install_opam_deps.sh
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo
COPY vendors /ligo/vendors
# install all transitive deps
RUN opam update && sh scripts/install_opam_deps.sh

# Install LIGO
COPY src /ligo/src
COPY dune /ligo
COPY dune-project /ligo/dune-project
COPY scripts/version.sh /ligo/scripts/version.sh
WORKDIR /ligo
# Version info and changelog
ARG ci_commit_tag
ARG ci_commit_sha
ARG ci_commit_timestamp
ENV CI_COMMIT_TAG=$ci_commit_tag
ENV CI_COMMIT_SHA=$ci_commit_sha
ENV CI_COMMIT_TIMESTAMP=$ci_commit_timestamp
COPY changelog.txt /ligo/changelog.txt
ENV CHANGELOG_PATH=/ligo/changelog.txt
RUN eval $(opam env) && LIGO_VERSION=$(/ligo/scripts/version.sh) dune build -p ligo --profile static

# Copy binary now to avoid problems with BISECT_ENABLE below
RUN cp /ligo/_build/install/default/bin/ligo /tmp/ligo

# Run tests
COPY gitlab-pages /ligo/gitlab-pages
RUN BISECT_ENABLE=yes opam exec -- dune runtest --profile static --no-buffer

# Coverage (only the overall)
RUN find . -name '*.coverage' | xargs rm -f
RUN opam exec -- dune runtest --instrument-with bisect_ppx --force
RUN opam exec -- bisect-ppx-report html -o coverage --title="LIGO test coverage"
RUN opam exec -- bisect-ppx-report summary --per-file > coverage/coverage-summary

# Run doc
RUN opam exec -- dune build @doc

# TODO see also ligo-docker-large in nix build
FROM alpine:3.12
WORKDIR /root/
COPY --from=0 /tmp/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/coverage /root/coverage
ENTRYPOINT ["/root/ligo"]
