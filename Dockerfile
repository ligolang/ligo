# I choose an OCaml image as the base image for no particular reason.
FROM ocaml/opam2:alpine-3.12-ocaml-4.09

USER root

# Now I will include the rust alpine Dockerfile here...
# https://github.com/rust-lang/docker-rust/blob/92915ac74d7cefc4eb063c8983f7bc27c3516e2e/1.44.0/alpine3.12/Dockerfile
RUN apk add --no-cache \
        ca-certificates \
        gcc

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    RUST_VERSION=1.44.0

RUN set -eux; \
    url="https://static.rust-lang.org/rustup/archive/1.21.1/x86_64-unknown-linux-musl/rustup-init"; \
    wget "$url"; \
    echo "0c86d467982bdf5c4b8d844bf8c3f7fc602cc4ac30b29262b8941d6d8b363d7e *rustup-init" | sha256sum -c -; \
    chmod +x rustup-init; \
    ./rustup-init -y --no-modify-path --profile minimal --default-toolchain $RUST_VERSION; \
    rm rustup-init; \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME; \
    rustup --version; \
    cargo --version; \
    rustc --version;
# End rust-lang/docker-rust Dockerfile

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev

# make bls12-381 build ???
ENV RUSTFLAGS='--codegen target-feature=-crt-static'

# Install opam deps
USER opam
COPY scripts/install_opam_deps.sh /ligo_prebuild/scripts/install_opam_deps.sh
WORKDIR /ligo_prebuild
COPY ligo.opam /ligo_prebuild
COPY ligo.opam.locked /ligo_prebuild
# copy all vendor .opams... this lets us install all transitive deps,
# but devs can change vendored code without invalidating the cache
COPY vendors/ParserLib/ParserLib.opam /ligo_prebuild/vendors/ParserLib/ParserLib.opam
COPY vendors/Red-Black_Trees/RedBlackTrees.opam /ligo_prebuild/vendors/Red-Black_Trees/RedBlackTrees.opam
COPY vendors/UnionFind/UnionFind.opam /ligo_prebuild/vendors/UnionFind/UnionFind.opam
COPY vendors/Preprocessor/Preprocessor.opam /ligo_prebuild/vendors/Preprocessor/Preprocessor.opam
COPY vendors/Michelson/Michelson.opam /ligo_prebuild/vendors/Michelson/Michelson.opam
COPY vendors/LexerLib/LexerLib.opam /ligo_prebuild/vendors/LexerLib/LexerLib.opam
COPY vendors/ligo-utils/proto-alpha-utils/proto-alpha-utils.opam /ligo_prebuild/vendors/ligo-utils/proto-alpha-utils/proto-alpha-utils.opam
COPY vendors/ligo-utils/tezos-utils/tezos-utils.opam /ligo_prebuild/vendors/ligo-utils/tezos-utils/tezos-utils.opam
COPY vendors/ligo-utils/memory-proto-alpha/tezos-memory-proto-alpha.opam /ligo_prebuild/vendors/ligo-utils/memory-proto-alpha/tezos-memory-proto-alpha.opam
COPY vendors/ligo-utils/simple-utils/simple-utils.opam /ligo_prebuild/vendors/ligo-utils/simple-utils/simple-utils.opam
RUN cd /home/opam/opam-repository && git pull && opam update -u -y # hmm, should just pick a desired SHA for reproducibility?
RUN opam switch 4.09 && eval $(opam env)
RUN sh scripts/install_opam_deps.sh

# Now install vendor libs
USER root
COPY vendors /ligo/vendors
COPY scripts/install_vendors_deps.sh /ligo/scripts/install_vendors_deps.sh
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo
RUN chown -R opam:opam /ligo
USER opam
WORKDIR /ligo
RUN sh scripts/install_vendors_deps.sh

# Version info env vars
ARG ci_job_id
ARG ci_commit_sha
ARG commit_date
ENV CI_JOB_ID=$ci_job_id
ENV CI_COMMIT_SHA=$ci_commit_sha
ENV COMMIT_DATE=$commit_date

# Install LIGO
USER root
COPY src /ligo/src
RUN chown -R opam:opam /ligo
USER opam
WORKDIR /ligo
# RUN opam install -y .
RUN eval $(opam env) && dune build -p ligo --profile static

# Run tests
USER root
COPY gitlab-pages /ligo/gitlab-pages
RUN chown -R opam:opam /ligo
USER opam
RUN opam exec -- dune runtest --profile static --no-buffer
RUN LIGO_FORCE_NEW_TYPER=true opam exec -- dune runtest --force --profile static --no-buffer

# Coverage (only the overall)
# _build needs to be clean before running test and bisect-ppx-report ..
RUN mv _build _build_
RUN BISECT_ENABLE=yes opam exec -- dune runtest --force --profile static --no-buffer
RUN opam exec -- bisect-ppx-report html -o coverage --title="LIGO test coverage"
RUN opam exec -- bisect-ppx-report summary --per-file > coverage/coverage-summary
RUN rm _build -fr && mv _build_ _build
# echo "Test coverage:"
# BISECT_ENABLE=yes dune runtest src/test --force
# bisect-ppx-report html -o $out/share/coverage/ligo --title="LIGO test coverage"
# echo "Doc coverage:"
# BISECT_ENABLE=yes dune build @doc-test --force
# bisect-ppx-report html -o $out/share/coverage/docs --title="LIGO doc coverage"
# echo "CLI test coverage:"
# BISECT_ENABLE=yes dune runtest src/bin/expect_tests
# bisect-ppx-report html -o $out/share/coverage/cli --title="CLI test coverage"

# Run doc
USER root
RUN chown -R opam:opam /ligo
USER opam
RUN opam exec -- dune build @doc

# TODO see also ligo-docker-large in nix build
FROM alpine:3.12
WORKDIR /root/
COPY --from=0 /ligo/_build/install/default/bin/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/coverage /root/coverage
CMD ["./ligo"]
