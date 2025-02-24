final: prev:
with prev; {
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope (_: prev:
        with prev; rec {
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
        });
    });
  coq_8_13 = coq_8_13.override {
    customOCamlPackages = final.ocaml-ng.ocamlPackages_4_14;
    buildIde = false;
  };
}
