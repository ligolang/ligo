(lang dune 3.8)

(name ligo)

(generate_opam_files true)

(source
 (gitlab ligolang/ligo))

(authors "Gabriel Alfour" "Christian Rinderknecht")

(maintainers
 "LigoLang <contact@ligolang.org>"
 "TriliTech <contact@trili.tech>")

(homepage "https://ligolang.org")

(bug_reports "https://gitlab.com/ligolang/ligo/issues")

(license MIT)

(using menhir 2.0)

(using coq 0.8)

(package
 (name ligo)
 (synopsis "A high-level language which compiles to Michelson")
 ; When modifying the dependencies, don't forget to run `make lock`
 (depends
  (core
    (>= v0.16.0))
  (core_unix
    (>= v0.16.0))
  (core_kernel
    (>= v0.16.0))
  ocamlgraph
  (menhir
   (= 20230608))
  bos
  qcheck
  terminal_size
  pprint
  yojson
  semver
  uri
  tls
  decompress
  tar
  tar-unix
  lambda-term
  parse-argv
  msgpck
  ppx_deriving
  ppx_deriving_yojson
  ppx_yojson_conv
  ppx_import
  (asetmap
   (= 0.8.1))
  (prometheus
   (>= 1.2))
  (lwt
   (= 5.7.0))
  lwt_ppx
  (bisect_ppx
   (>= 2.3.0))
  irmin
  (cmdliner
   (= 1.1.0))
  ocaml-compiler-libs
  simple-diff
  stdint
  (ocaml-migrate-parsetree
   (= 2.3.0))
  alcotest-lwt
  qcheck-alcotest
  irmin-pack
  pure-splitmix
  cohttp-server-lwt-unix
  resto-cohttp-self-serving-client
  tezos-rust-libs
  crunch
  class_group_vdf
  hex
  lwt-canceler
  seqes
  ctypes_stubs_js
  hacl-star-raw
  hacl-star
  secp256k1-internal
  (mtime
   (>= 2.0.0))
  (zarith
   (>= 1.4))
  (tezt
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (ocamlformat
   (= 0.21.0))
  (ocamlformat-rpc
   (= 0.21.0))
  (ocaml-lsp-server
   (>= 1.14.0))
  (lsp
   (and
    (>= 1.17)
    (< 1.18)))
  aches-lwt
  fileutils
  (conduit
   (= 6.1.0))
  (ocaml-recovery-parser
   (= 0.2.4))
  (linol
   (= 0.5))
  (linol-lwt
   (= 0.5))
  data-encoding
  ; For building, testing, or documentation 
  (dune
   (>= 3.8 :build))
  (coq
   (and
    (>= 8.12)
    (< 8.14)
    :build))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
  (odoc :with-doc)
  ; For dev
  (ocamlformat
   (= 0.21.0 :dev))
  (ocaml-lsp-server
   (>= 1.14.0 :dev))
  (ocamlformat-rpc
   (= 0.21.0 :dev))))
