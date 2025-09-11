final: prev:
with prev; {
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_2 = ocamlPackages_5_2.overrideScope (_: prev:
        with prev; rec {
          octezSource = fetchFromGitLab {
            owner = "ligolang";
            repo = "tezos-ligo";
            rev = "4d1f2bc8cdc13690328ead815dae7219561b38e5";
            hash = "sha256-ureC2EegBNOPtEpMlJYvnWbesg2Z6TX31UzshXW0yds=";
          };
          bls12-381 = 
            buildDunePackage rec {
              pname = "bls12-381";
              version = "v23-ligo";
              src = octezSource;

              propagatedBuildInputs = [
                integers zarith hex tezt
              ];
            };
          buildOctezPackage = {
            pname,
            propagatedBuildInputs ? [],
            nativeBuildInputs ? [],
          }:
            buildDunePackage {
              pname = pname;
              version = "v23-ligo";
              src = octezSource;
              nativeBuildInputs = nativeBuildInputs;
              propagatedBuildInputs =
                [
                  ppxlib
                  logs
                  ppx_repr
                  digestif
                  zarith
                  mtime
                  lwt
                  rusage
                  astring
                  checkseum
                  cmdliner
                  index
                  bigstringaf
                  uri
                  ocamlgraph
                  bheap
                  pure-splitmix
                  bls12-381
                  conduit-lwt-unix
                  qcheck-alcotest
                  asetmap
                  lwt-watcher
                  tezt
                  ppx_expect
                  alcotest-lwt
                  aches
                  hacl-star
                  seqes
                  stdint
                  cohttp
                  camlp-streams
                  secp256k1-internal
                  lwt-canceler
                  lwt-exit
                  magic-mime
                  aches-lwt
                  tar-unix
                  dune-configurator
                  camlzip
                  yaml
                  ppx_import
                  ctypes
                  ctypes-foreign
                  class_group_vdf
                  pprint
                  ocp-ocamlres
                  pyml
                  libiconv
                  lwt_ppx
                  opentelemetry
                  opentelemetry-lwt
                  ambient-context
                  ambient-context-lwt
                  eio
                  eio_main
                  lwt_eio
                  caqti-driver-sqlite3
                  ledgerwallet
                  ledgerwallet-tezos
                ]
                ++ propagatedBuildInputs
                ++ lib.optionals stdenv.isDarwin [
                  darwin.apple_sdk.frameworks.Security
                ];
            };
          cohttp = buildDunePackage rec {
            pname = "cohttp";
            version = "5.3.1";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-cohttp/releases/download/v${version}/cohttp-${version}.tbz";
              hash = "sha256-9eJz08Lyn/R71+Ftsj4fPWzQGkC+ACCJhbxDTIjUV2s=";
            };
            postPatch = ''
              substituteInPlace cohttp/src/dune --replace 'bytes base64' 'base64'
            '';
            buildInputs = [jsonm ppx_sexp_conv];
            propagatedBuildInputs = [base64 re stringext uri-sexp];
          };
          opentelemetry = buildDunePackage rec {
            pname = "opentelemetry";
            version = "0.11.2";
            src = fetchurl {
              url = "https://github.com/imandra-ai/ocaml-opentelemetry/releases/download/v${version}/opentelemetry-${version}.tbz";
              hash = "sha256-cWp0B9y7jZUClfVdK3L+wUvxIlWJcmgdZrSlY1KYfBw=";
            };
            postPatch = ''
              substituteInPlace src/ambient-context/dune --replace 'atomic' ' '
            '';
            propagatedBuildInputs = [pbrt thread-local-storage hmap ptime ambient-context lwt];
          };
          opentelemetry-lwt = buildDunePackage rec {
            pname = "opentelemetry-lwt";
            version = "0.11.2";
            src = opentelemetry.src;
            postPatch = opentelemetry.postPatch;
            propagatedBuildInputs = [opentelemetry lwt lwt_ppx ambient-context ambient-context-lwt];
          };
          ambient-context = buildDunePackage rec {
            pname = "ambient-context";
            version = "0.1.0";
            src = fetchurl {
              url = "https://github.com/ELLIOTTCABLE/ocaml-ambient-context/archive/refs/tags/v${version}.tar.gz";
              hash = "sha256-8GZ7dbwJZfFnltszrs3XZSEC5t1bUNC2tirUii5z5GY=";
            };
          };
          ambient-context-lwt = buildDunePackage rec {
            pname = "ambient-context-lwt";
            version = "0.1.0";
            src = ambient-context.src;
            propagatedBuildInputs = [ambient-context lwt];
          };
          pbrt = buildDunePackage rec {
              pname = "pbrt";
              version = "3.1.1";
              src = fetchurl {
                url = "https://github.com/mransan/ocaml-protoc/releases/download/v${version}/ocaml-protoc-${version}.tbz";
                hash = "sha256-xWV/y/y66jYb64R/crimpvNs6edzvyhbJ4oNp1+Yj7w=";
              };
            };
          tar = buildDunePackage rec {
            pname = "tar";
            version = "2.6.0";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-tar/releases/download/v${version}/tar-${version}.tbz";
              hash = "sha256-yv8MtwRjQ+K/9/wPkhfk4xI1VV5MSIn7GUeSmFtvse4=";
            };
            propagatedBuildInputs = [ camlp-streams decompress cstruct ];
          };
          tar-unix = buildDunePackage {
            pname = "tar-unix";
            inherit (tar) version src;

            propagatedBuildInputs = [ tar lwt cstruct-lwt ];
          };
          zarith = prev.zarith.overrideAttrs (prev: rec {
            version = "1.13";
            src = fetchFromGitHub {
              owner = "ocaml";
              repo = "Zarith";
              rev = "release-${version}";
              hash = "sha256-CNVKoJeO3fsmWaV/dwnUA8lgI4ZlxR/LKCXpCXUrpSg=";
            };
          });
          ocaml-recovery-parser = buildDunePackage
            rec {
              pname = "ocaml-recovery-parser";
              version = "0.3.0";

              duneVersion = "3";

              src = fetchFromGitHub {
                owner = "serokell";
                repo = pname;
                rev = version;
                sha256 = "sha256-RFRI7VoHd7GceIQnzN1FQYfR/5nbrU/t1pLTbURE6PY=";
              };

              propagatedBuildInputs = [
                fix
                menhirLib
                menhirSdk
              ];
            };
          grace = buildDunePackage rec {
            pname = "grace";
            version = "0.0.2";
            src = fetchFromGitHub {
              owner = "johnyob";
              repo = "grace";
              rev = "d15a6d7d07a2551d1a9934fa79c2cf84c918f990";
              hash = "sha256-jubzimeKs29Y6Di2/kpKEOnNAEzMzVpC5HMLjog4Tlg=";
            };
            propagatedBuildInputs = [core ppx_jane fmt dedent iter core_unix uutf ppx_optcomp];
          };

          tezt = buildDunePackage rec {
            pname = "tezt";
            version = "4.1.0";

            src = fetchFromGitLab {
              owner = "nomadic-labs";
              repo = pname;
              rev = version;
              hash = "sha256-1Cl/GOB+MDPJIl/6600PLTSL+vCYcAZGjedd6hr7rJw=";
            };

            propagatedBuildInputs = [clap ezjsonm lwt re];
          };

          stdcompat = buildDunePackage {
            pname = "stdcompat";
            version = "19";

            src = fetchFromGitHub {
              owner = "thierry-martinez";
              repo = "stdcompat";
              # patched 19, required by tezos
              rev = "d53390d788027fe0a2282c4745eb3d1626341f99";
              hash = "sha256-94DM61C7r8zZ3AUfZd2aTvaxMiAVC585F2A9hSF4YPY=";
            };

            dontConfigure = true;
          };
          # TODO: this is weird to be required
          ledgerwallet = buildDunePackage rec {
            pname = "ledgerwallet";
            version = "0.4.1";
            src = fetchurl {
              url = "https://github.com/vbmithr/ocaml-ledger-wallet/archive/${version}.tar.gz";
              hash = "sha256-0UejqUZ55Nlcx/mM32lu2kNdzsNsz3qXWe65vfuea50=";
            };
            propagatedBuildInputs = [ rresult cstruct hidapi-lwt lwt ];
          };
          ledgerwallet-tezos = buildDunePackage {
            pname = "ledgerwallet-tezos";
            inherit (ledgerwallet) version src;

            propagatedBuildInputs = [ ledgerwallet ];
          };
          hidapi-lwt = buildDunePackage {
            pname = "hidapi-lwt";
            inherit (hidapi) version src nativeBuildInputs buildInputs;
            propagatedBuildInputs = [ hidapi lwt ];
          };
          octez-rustzcash-deps = buildDunePackage {
            pname = "octez-rustzcash-deps";
            version = "v23-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
              outputHashes = {
                "octez-riscv-0.0.0" = "sha256-7TxDp0gltdoAC1Yhbb/roPbHBZYirlgcBaFROtYJYWw=";
                "tezos-smart-rollup-build-utils-0.2.2" = "sha256-Z6Z3Jti5J4YzDKdsaZ5i/YdaSTctbPGmj5nMlOG7RuA=";
              };
            };
            postPatch = ''
              cd src/rustzcash_deps
              cargo update
              find . -type d -exec chmod u+w {} +
              patchShebangs .
              cd ../..
            '';
            propagatedBuildInputs =
              [libiconv]
              ++ lib.optionals stdenv.isDarwin [
                darwin.apple_sdk.frameworks.Security
              ];
            nativeBuildInputs = [
              rustc
              cargo
              rustPlatform.cargoSetupHook
            ];
          };
          octez-rust-deps = buildDunePackage {
            pname = "octez-rust-deps";
            version = "v23-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
              outputHashes = {
                "octez-riscv-0.0.0" = "sha256-7TxDp0gltdoAC1Yhbb/roPbHBZYirlgcBaFROtYJYWw=";
                "tezos-smart-rollup-build-utils-0.2.2" = "sha256-Z6Z3Jti5J4YzDKdsaZ5i/YdaSTctbPGmj5nMlOG7RuA=";
              };
            };
            postPatch = ''
              cd src/rust_deps
              find . -type d -exec chmod u+w {} +
              patchShebangs .
              cd ../..
            '';
            propagatedBuildInputs =
              [libiconv]
              ++ lib.optionals stdenv.isDarwin [
                darwin.apple_sdk.frameworks.Security
              ];
            nativeBuildInputs = [
              rustc
              cargo
              rustPlatform.cargoSetupHook
            ];
          };
          octez-alcotezt = buildOctezPackage {
            pname = "octez-alcotezt";
          };
          octez-internal-libs = buildOctezPackage {
            pname = "octez-internal-libs";
            propagatedBuildInputs = [octez-alcotezt];
          };
          octez-distributed-internal = buildOctezPackage {
            pname = "octez-distributed-internal";
          };
          octez-distributed-lwt-internal = buildOctezPackage {
            pname = "octez-distributed-lwt-internal";
            propagatedBuildInputs = [octez-distributed-internal];
          };
          octez-libs = buildOctezPackage {
            pname = "octez-libs";
            propagatedBuildInputs = [
              octez-rust-deps
              octez-internal-libs
              octez-distributed-lwt-internal
            ];
          };
          octez-version = buildOctezPackage {
            pname = "octez-version";
            propagatedBuildInputs = [octez-libs];
          };
          octez-riscv-api = buildOctezPackage {
            pname = "octez-riscv-api";
            propagatedBuildInputs = [octez-libs];
          };
          octez-riscv-pvm = buildOctezPackage {
            pname = "octez-riscv-pvm";
            propagatedBuildInputs = [octez-riscv-api];
          };
          octez-performance-metrics = buildOctezPackage {
            pname = "octez-performance-metrics";
            propagatedBuildInputs = [octez-libs];
          };
          octez-l2-libs = buildDunePackage {
            pname = "octez-l2-libs";
            propagatedBuildInputs = [
              octez-libs
              octez-rust-deps
              caqti-lwt
              octez-riscv-pvm
              octez-performance-metrics
            ];
            version = "v23-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
              outputHashes = {
                "octez-riscv-0.0.0" = "sha256-7TxDp0gltdoAC1Yhbb/roPbHBZYirlgcBaFROtYJYWw=";
                "tezos-smart-rollup-build-utils-0.2.2" = "sha256-Z6Z3Jti5J4YzDKdsaZ5i/YdaSTctbPGmj5nMlOG7RuA=";
              };
            };
            postPatch = ''
              cd src/rust_deps
              find . -type d -exec chmod u+w {} +
              patchShebangs .
              cd ../..
            '';
            nativeBuildInputs = [
              rustc
              cargo
              rustPlatform.cargoSetupHook
              crunch
            ];
          };
          octez-proto-libs = buildOctezPackage {
            pname = "octez-proto-libs";
            propagatedBuildInputs = [octez-l2-libs];
          };
          octez-protocol-compiler-compat = buildOctezPackage {
            pname = "octez-protocol-compiler-compat";
          };
          octez-protocol-compiler = buildOctezPackage {
            pname = "octez-protocol-compiler";
            nativeBuildInputs = [ocp-ocamlres];
            propagatedBuildInputs = [
              octez-version
              octez-proto-libs
              octez-protocol-compiler-compat
            ];
          };
          pringo = stdenv.mkDerivation rec {
            pname = "pringo";
            version = "1.4.0";
            name = "${ocaml.version}-${pname}-${version}";
            src = fetchFromGitHub {
              owner = "xavierleroy";
              repo = "pringo";
              rev = "d3e17e4b8bbe22cb74fa1f4de7a5862322f40369";
              hash = "sha256-aagxV94GXnX9ghfwKo1tf9z097rDZ1E0edekrMBYjAk=";
            };
            dontAddStaticConfigureFlags = true;
            createFindlibDestdir = true;
            strictDeps = true;
            nativeBuildInputs = [ocaml findlib];
          };
          prbnmcn-basic-structures = buildDunePackage {
            pname = "prbnmcn-basic-structures";
            version = "0.0.1";
            src = fetchFromGitHub {
              owner = "igarnier";
              repo = "prbnmcn-basic-structures";
              rev = "4f11ba67965bef101763fb167c4eed1cd967ecf8";
              hash = "sha256-0lcGsL+rrc13ZwfzfAneLwJVoi0MbPjOEhQiveexOco=";
            };
            propagatedBuildInputs = [zarith];
          };
          prbnmcn-stats = buildDunePackage {
            pname = "prbnmcn-stats";
            version = "0.0.8";
            src = fetchFromGitHub {
              owner = "igarnier";
              repo = "prbnmcn-stats";
              rev = "38299af39a1d628bfbdc1fa48ec18823e8cddfaa";
              hash = "sha256-YajRUp8tdoMyqpBV17/v812bjmlrXt1HrDQQNpVG6/A=";
            };
            propagatedBuildInputs = [prbnmcn-basic-structures];
          };
          prbnmcn-linalg = buildDunePackage {
            pname = "prbnmcn-linalg";
            version = "0.0.1";
            src = fetchFromGitHub {
              owner = "igarnier";
              repo = "prbnmcn-linalg";
              rev = "54c7fd251143b2311034d1f36e7fe5bdbba9bbf8";
              hash = "sha256-nr7W5TRkgTnkYEiWZIDcXcsUIswzDZf5CrhFu7kEAt0=";
            };
            propagatedBuildInputs = [prbnmcn-basic-structures];
          };
          tezos-benchmark = buildOctezPackage {
            pname = "tezos-benchmark";
            propagatedBuildInputs = [
              octez-libs
              pringo
              prbnmcn-stats
              prbnmcn-linalg
            ];
          };
          octez-shell-libs = buildOctezPackage {
            pname = "octez-shell-libs";
            propagatedBuildInputs = [
              octez-protocol-compiler
              tezos-benchmark
              octez-rustzcash-deps
            ];
          };
          octez-crawler = buildOctezPackage {
            pname = "octez-crawler";
            propagatedBuildInputs = [octez-shell-libs];
          };
          octez-injector = buildOctezPackage {
            pname = "octez-injector";
            propagatedBuildInputs = [octez-crawler];
          };
          tezos-protocol-alpha = buildOctezPackage {
            pname = "tezos-protocol-alpha";
            nativeBuildInputs = [octez-protocol-compiler];
            propagatedBuildInputs = [octez-proto-libs octez-shell-libs];
          };
          tezos-dal-node-services = buildOctezPackage {
            pname = "tezos-dal-node-services";
            propagatedBuildInputs = [octez-shell-libs];
          };
          dal_node_migrations = buildOctezPackage {
            pname = "dal_node_migrations";
            propagatedBuildInputs = [octez-l2-libs];
            nativeBuildInputs = [ crunch ];
          };
          tezos-dal-node-lib = buildOctezPackage {
            pname = "tezos-dal-node-lib";
            propagatedBuildInputs = [
              tezos-dal-node-services
              dal_node_migrations
              octez-crawler
            ];
          };
          tezt-tezos = buildOctezPackage {
            pname = "tezt-tezos";
            propagatedBuildInputs = [octez-libs];
          };
          octez-node-config = buildOctezPackage {
            pname = "octez-node-config";
            propagatedBuildInputs = [octez-shell-libs];
          };
          octez-baker-lib = buildOctezPackage {
            pname = "octez-baker-lib";
            propagatedBuildInputs = [
              octez-shell-libs
              tezos-dal-node-lib
              octez-node-config
            ];
          };
          octez-protocol-alpha-libs = buildOctezPackage {
            pname = "octez-protocol-alpha-libs";
            propagatedBuildInputs = [
              octez-injector
              tezos-protocol-alpha
              tezt-tezos
              octez-baker-lib
              memtrace
            ];
          };
        });
    });
  tree-sitter = tree-sitter.override (
    # override for getting tree-sitter version 0.25.3
    let
      rp = rustPlatform;
    in
      rec {
        # dont want rust Playground
        webUISupport = false;
        rustPlatform = rp // {
          buildRustPackage = args: rp.buildRustPackage (
            args // rec {
              version = "0.25.3";
              src = fetchFromGitHub {
                owner = "tree-sitter";
                repo = "tree-sitter";
                rev = "v${version}";
                hash = "sha256-xafeni6Z6QgPiKzvhCT2SyfPn0agLHo47y+6ExQXkzE";
                fetchSubmodules = true;
              };
              cargoHash = "sha256-rjUn8F6WSxLQGrFzK23q4ClLePSpcMN2+i7rC02Fisk=";
              nativeBuildInputs = args.nativeBuildInputs ++ [
                # needs rustc 1.82
                final.rust-bin.stable.latest.default
              ];
              # NOTE still need to patch as we dont want the playground support,
              # but the treesitter src code has changed so need an updated patch file
              patches = lib.optionals (!webUISupport) [
                (substitute {
                  src = ./tree-sitter-remove-web-interface.patch;
                })
              ];
            }
          );
        };
      });
}
