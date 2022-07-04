FROM esydev/esy:nightly-alpine-latest

RUN apk add emacs eudev-dev libusb-dev
COPY . /app
WORKDIR /app
RUN esy @static.esy i --ocaml-pkg-name ocaml --ocaml-version 4.10.1002-musl.static.flambda
RUN esy @static.esy build-dependencies --release --ocaml-pkg-name ocaml --ocaml-version 4.10.1002-musl.static.flambda && rm -rf ~/.esy/3/b
RUN esy @static.esy b --release --ocaml-pkg-name ocaml --ocaml-version 4.10.1002-musl.static.flambda
RUN esy @static.esy release --static --ocaml-pkg-name ocaml --ocaml-version 4.10.1002-musl.static.flambda
WORKDIR /app/_release
RUN npm pack
