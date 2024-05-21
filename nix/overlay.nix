{
  opam-repository,
  linol,
  ocaml-recovery-parser,
  tezos-ligo,
}: final: prev: let
  inherit (prev) lib stdenv libiconv;

  fix-conf-rust = final: prev: {
    conf-rust-2021 = prev.conf-rust-2021.overrideAttrs (old: {
      propagatedNativeBuildInputs =
        (old.propagatedNativeBuildInputs or [])
        ++
        # Upstream conf-rust* packages don't request libiconv
        [libiconv];
    });
  };

  increase-jobs-for-ocaml = final: prev:
    lib.optionalAttrs (lib.hasAttr "ocaml-base-compiler" prev) {
      ocaml-base-compiler = prev.ocaml-base-compiler.override {
        # Compile faster!
        jobs = "$NIX_BUILD_CORES";
      };
    };

  patch-submodules-for-ligo = final: prev: {
    ligo = prev.ligo.overrideAttrs (old: {
      postPatch =
        (old.postPatch or "")
        + ''
          mkdir -p vendors/tezos-ligo
          cp -r ${tezos-ligo}/. vendors/tezos-ligo/
        '';
    });
  };
in {
  # fixed point on opam-nix-integration instead of ocamlPackages since
  # the opam-nix-integration overlay defines ocamlPackages as:
  # final: prev: {
  #   opam-nix-integration = ...;
  #   ocamlPackages = final.opam-nix-integration;
  # }
  opam-nix-integration = prev.opam-nix-integration.overrideScope (
    lib.composeManyExtensions [
      (final: prev: {
        repository = prev.repository.override {src = opam-repository;};
      })

      # Select packages
      (final: prev:
        prev.repository.select {
          opams = [
            {
              name = "data-encoding";
              version = "1.0.1";
              opam = "${tezos-ligo}/data-encoding/data-encoding.opam";
              src = "${tezos-ligo}/data-encoding";
            }

            {
              name = "ligo";
              opam = ../ligo.opam.locked;
              src = ../.;
            }

            {
              name = "linol";
              version = "0.5";
              opam = "${linol}/linol.opam";
              src = "${linol}";
            }

            {
              name = "linol-lwt";
              version = "0.5";
              opam = "${linol}/linol-lwt.opam";
              src = "${linol}";
            }

            {
              name = "ocaml-recovery-parser";
              version = "0.2.4";
              opam = "${ocaml-recovery-parser}/ocaml-recovery-parser.opam";
              src = "${ocaml-recovery-parser}";
            }
          ];
        })

      # Patch the git submodules for ligo
      patch-submodules-for-ligo

      # Increase jobs for building OCaml compiler
      increase-jobs-for-ocaml

      # Fix conf-rust
      fix-conf-rust
    ]
  );
}
