{
  stdenv,
  lib,
  pkgs,
  tezos-ligo,
}: let
  inherit (pkgs) darwin ocamlPackages python3Packages coq_8_13 tezos-rust-libs;
in
  with ocamlPackages;
    buildDunePackage rec {
      pname = "ligo";
      version = "dev";
      src = ./..;

      OPAM_SWITCH_PREFIX = "${tezos-rust-libs}";

      postPatch = ''
        mkdir -p vendors/tezos-ligo
        cp -r ${tezos-ligo}/. vendors/tezos-ligo/
      '';

      nativeBuildInputs = [
        menhir
        ocaml-recovery-parser
        coq_8_13
        crunch
        odoc
        python3Packages.jsonschema
      ];

      propagatedBuildInputs =
        [
          core
          core_unix
          core_kernel
          ocamlgraph
          menhir
          menhirLib
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
          asetmap
          prometheus
          lwt
          lwt_ppx
          bisect_ppx
          irmin
          cmdliner
          ocaml-compiler-libs
          simple-diff
          stdint
          ocaml-migrate-parsetree-2
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
          mtime
          zarith
          tezt
          lsp
          aches-lwt
          fileutils
          conduit
          ocaml-recovery-parser
          linol
          linol-lwt
          data-encoding
          dune-configurator # ???
          coq_8_13 # ???
          alcotest # with-test
          ppx_expect # with-test
          ppx_inline_test # with-test
        ]
        ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.Security
        ];
    }
