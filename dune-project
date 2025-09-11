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

(using ctypes 0.3)

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
  (tar
   (= 2.6.0))
  (tar-unix
   (= 2.6.0))
  lambda-term
  msgpck
  (bin_prot
   (>= v0.16.0))
  (ppx_bin_prot
   (>= v0.16.0))
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
  lwt_ssl
  (bisect_ppx
   (>= 2.3.0))
  irmin
  (cmdliner
   (= 1.3.0))
  ocaml-compiler-libs
  simple-diff
  stdint
  alcotest-lwt
  qcheck-alcotest
  irmin-pack
  pure-splitmix
  ;  cohttp-server-lwt-unix
  ;  resto-cohttp-self-serving-client
  crunch
  class_group_vdf
  hex
  lwt-canceler
  lwt-exit
  logs
  (cohttp
   (= 5.3.1))
  conduit-lwt-unix
  magic-mime
  seqes
  ctypes_stubs_js
  ctypes
  ctypes-foreign
  hacl-star-raw
  hacl-star
  secp256k1-internal
  (mtime
   (>= 2.0.0))
  (zarith
   (= 1.13))
  (tezt
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (ocaml-lsp-server
   (>= 1.21.0))
  aches-lwt
  fileutils
  (conduit
   (= 8.0.0))
  (ocaml-recovery-parser
   (= 0.3.0))
  (linol
   (= 0.10))
  (linol-lwt
   (= 0.10))
  grace
  eio
  eio_main
  lwt_eio
  ; For building, testing, or documentation
  (dune
   (>= 3.8 :build))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
  (odoc :with-test)
  ; For dev
  (ocamlformat
   (= 0.27.0 :dev))
  (ocamlformat-rpc
   (= 0.27.0 :dev))
  (ocaml-lsp-server
   (>= 1.21.0 :dev))))
