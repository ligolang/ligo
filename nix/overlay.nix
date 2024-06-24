final: prev:
with prev; {
  libsodium = libsodium.overrideAttrs (with libsodium; rec {
    version = "1.0.18";
    src = final.fetchurl {
      url = "https://download.libsodium.org/libsodium/releases/${pname}-${version}.tar.gz";
      hash = "sha256-b1BEkLNCpPikxKAvybhmy++GItXfTlRStGvhIeRmNsE=";
    };
  });
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope' (_: prev:
        with prev; rec {
          http = buildDunePackage rec {
            pname = "http";
            version = "6.0.0_beta2";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-cohttp/releases/download/v${version}/cohttp-v${version}.tbz";
              hash = "sha256-kOzsi9WAQRtCcsAxsva5wKUEhdIGg8apxhUkLzcksBc=";
            };
          };
          cohttp-server-lwt-unix = buildDunePackage {
            inherit (http) version src;
            pname = "cohttp-server-lwt-unix";
            propagatedBuildInputs = [lwt http];
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
        });
    });
  coq_8_13 = coq_8_13.override {
    customOCamlPackages = final.ocaml-ng.ocamlPackages_4_14;
  };
  tezos-rust-libs = prev.tezos-rust-libs.overrideAttrs (_: {
    version = "1.7";
    src = fetchFromGitLab {
      owner = "tezos";
      repo = "tezos-rust-libs";
      rev = "v1.7";
      sha256 = "sha256-L+8qu3DXqru5AeQWSC8Eeii+OTZnYbpw6X05K+EapNE=";
    };

    buildPhase = ''
      runHook preBuild

      cargo build \
        --target-dir target-librustzcash \
        --package librustzcash \
        --release

      cargo build \
        --target-dir target-wasmer \
        --package wasmer-c-api \
        --no-default-features \
        --features singlepass,cranelift,wat,middlewares \
        --release

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/lib/tezos-rust-libs/rust
      cp "librustzcash/include/librustzcash.h" \
          "target-librustzcash/release/librustzcash.a" \
          "wasmer-3.3.0/lib/c-api/wasm.h" \
          "wasmer-3.3.0/lib/c-api/wasmer.h" \
          "target-wasmer/release/libwasmer.a" \
          "$out/lib/tezos-rust-libs"
      cp -r "librustzcash/include/rust" "$out/lib/tezos-rust-libs"

      runHook postInstall
    '';
  });
  boehmgc = boehmgc.overrideAttrs {
    # tests for this sometimes fails on macOS
    doCheck = !prev.stdenv.isDarwin;
  };
}
