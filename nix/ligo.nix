{
  stdenv,
  lib,
  pkgs,
  tezos-ligo,
  tree-sitter,
  tree-sitter-typescript,
  grace,
  lltz,
  libiconv
}: let
  inherit (pkgs) darwin ocamlPackages python3Packages coq_8_13;
in
  with ocamlPackages;
    buildDunePackage rec {
      pname = "ligo";
      version = "dev";
      src = ./..;

      TREE_SITTER = "${tree-sitter}";
      TREE_SITTER_TYPESCRIPT = "${tree-sitter-typescript}";

      postPatch = ''
        mkdir -p vendors/tezos-ligo
        cp -r ${tezos-ligo}/. vendors/tezos-ligo/
        mkdir -p vendors/grace
        cp -r ${grace}/. vendors/grace/
        mkdir -p vendors/lltz
        cp -r ${lltz}/. vendors/lltz/
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
          tree-sitter
          tree-sitter-typescript
          libiconv
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
          tezt
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
          crunch
          class_group_vdf
          hex
          lwt-canceler
          lwt-exit
          seqes
          ctypes_stubs_js
          hacl-star-raw
          hacl-star
          secp256k1-internal
          mtime
          zarith
          lsp
          aches-lwt
          fileutils
          conduit
          ocaml-recovery-parser
          linol
          linol-lwt
          dune-configurator # ???
          coq_8_13 # ???
          alcotest # with-test
          ppx_expect # with-test
          ppx_inline_test # with-test
          ctypes
          ctypes-foreign
          logs
          cohttp
          conduit-lwt-unix
          magic-mime
        ]
        ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.Security
        ];
    }
