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
          cohttp = buildDunePackage rec {
            pname = "cohttp";
            version = "5.3.1";
            minimalOCamlVersion = "4.08";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-cohttp/releases/download/v${version}/cohttp-${version}.tbz";
              hash = "sha256-9eJz08Lyn/R71+Ftsj4fPWzQGkC+ACCJhbxDTIjUV2s=";
            };
            buildInputs = [ jsonm ppx_sexp_conv ];
            propagatedBuildInputs = [ base64 re stringext uri-sexp ];
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

            propagatedBuildInputs = [ clap ezjsonm lwt re ];
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
  
  boehmgc = boehmgc.overrideAttrs {
    # tests for this sometimes fails on macOS
    doCheck = !prev.stdenv.isDarwin;
  };
  perlPackages = prev.perlPackages // {
    libnet = prev.perlPackages.libnet.overrideAttrs (oldAttrs: {
      doCheck = false;
    });
  };
  indent = prev.indent.overrideAttrs (oldAttrs: rec {
    doCheck = false;
  });
  bison = prev.bison.overrideAttrs (oldAttrs: {
    doCheck = false;
  });
  p11-kit = prev.p11-kit.overrideAttrs (oldAttrs: {
    doCheck = false;
  });
}
