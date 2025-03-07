final: prev:
with prev; {
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope (_: prev:
        with prev; rec {
          octezSource = fetchFromGitLab {
            owner = "ligolang";
            repo = "tezos-ligo";
            rev = "fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30";
            hash = "sha256-FkOR4VqFngdejBCqzwwySINfK/oNklsnIVrvQYVAfyc=";
          };
          buildOctezPackage = {
            pname,
            propagatedBuildInputs ? [],
            nativeBuildInputs ? [],
          }:
            buildDunePackage {
              pname = pname;
              version = "v21-ligo";
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
                  ocaml-migrate-parsetree-2
                  ocp-ocamlres
                  pyml
                  libiconv
                ]
                ++ propagatedBuildInputs
                ++ lib.optionals stdenv.isDarwin [
                  darwin.apple_sdk.frameworks.Security
                ];
            };
          cohttp = buildDunePackage rec {
            pname = "cohttp";
            version = "5.3.1";
            minimalOCamlVersion = "4.08";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-cohttp/releases/download/v${version}/cohttp-${version}.tbz";
              hash = "sha256-9eJz08Lyn/R71+Ftsj4fPWzQGkC+ACCJhbxDTIjUV2s=";
            };
            buildInputs = [jsonm ppx_sexp_conv];
            propagatedBuildInputs = [base64 re stringext uri-sexp];
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

            minimalOCamlVersion = "4.12";

            src = fetchFromGitLab {
              owner = "nomadic-labs";
              repo = pname;
              rev = version;
              hash = "sha256-1Cl/GOB+MDPJIl/6600PLTSL+vCYcAZGjedd6hr7rJw=";
            };

            propagatedBuildInputs = [clap ezjsonm lwt re];
          };

          # TODO: odoc-parser and ocamlformat are issues with nix-ocaml
          odoc-parser = prev.odoc-parser.overrideAttrs (prev: {
            propagatedBuildInputs = (prev.propagatedBuildInputs or []) ++ [result];
            postPatch = "";
          });
          ocamlformat_0_21_0 = prev.ocamlformat_0_21_0.overrideAttrs (prev: rec {
            version = "0.21.0";
            tarballName = "ocamlformat-${version}.tbz";
            src = final.fetchurl {
              url = "https://github.com/ocaml-ppx/ocamlformat/releases/download/${version}/${tarballName}";
              sha256 = "sha256-KhgX9rxYH/DM6fCqloe4l7AnJuKrdXSe6Y1XY3BXMy0=";
            };
            propagatedBuildInputs = [csexp];
          });

          octez-src = fetchFromGitLab {
            owner = "ligolang";
            repo = "tezos-ligo";
            rev = "fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30";
            hash = "sha256-FkOR4VqFngdejBCqzwwySINfK/oNklsnIVrvQYVAfyc=";
          };
          octez-rust-deps = buildDunePackage {
            pname = "octez-rust-deps";
            version = "v21-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
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
          octez-l2-libs = buildDunePackage {
            pname = "octez-l2-libs";
            propagatedBuildInputs = [octez-libs octez-riscv-pvm];
            version = "v21-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
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
            ];
          };
          octez-proto-libs = buildOctezPackage {
            pname = "octez-proto-libs";
            propagatedBuildInputs = [octez-l2-libs];
          };
          octez-protocol-compiler = buildOctezPackage {
            pname = "octez-protocol-compiler";
            nativeBuildInputs = [ocp-ocamlres];
            propagatedBuildInputs = [octez-version octez-proto-libs];
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
            propagatedBuildInputs = [octez-protocol-compiler tezos-benchmark];
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
          tezos-dal-node-lib = buildOctezPackage {
            pname = "tezos-dal-node-lib";
            propagatedBuildInputs = [tezos-dal-node-services];
          };
          tezos-dac-lib = buildOctezPackage {
            pname = "tezos-dac-lib";
            propagatedBuildInputs = [octez-shell-libs];
          };
          tezos-dac-client-lib = buildOctezPackage {
            pname = "tezos-dac-client-lib";
            propagatedBuildInputs = [tezos-dac-lib];
          };
          tezt-tezos = buildOctezPackage {
            pname = "tezt-tezos";
            propagatedBuildInputs = [octez-libs];
          };
          octez-protocol-alpha-libs = buildOctezPackage {
            pname = "octez-protocol-alpha-libs";
            propagatedBuildInputs = [
              octez-injector
              tezos-protocol-alpha
              tezos-dal-node-lib
              tezos-dac-client-lib
              tezt-tezos
            ];
          };
        });
    });
  coq_8_13 = coq_8_13.override {
    customOCamlPackages = final.ocaml-ng.ocamlPackages_4_14;
    buildIde = false;
  };
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
              cargoHash = "sha256-CPLUHIihMSF8pTve0cJYt5Pb71zNZH2/d35bCKtp8w8";
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
