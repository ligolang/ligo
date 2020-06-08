# An overlay that adds ligo to ocamlPackages

{ sources ? import ./sources.nix
, CI_COMMIT_SHA ? builtins.getEnv "CI_COMMIT_SHA"
, COMMIT_DATE ? builtins.getEnv "COMMIT_DATE" }:
self: super:
let
  opam-nix = import sources.opam-nix (import sources.nixpkgs { });
  inherit (import sources."gitignore.nix" { inherit (self) lib; })
    gitignoreSource;
  # Remove list of directories or files from source (to stop unneeded rebuilds)
  # Also, apply the gitignore here.
  filterOut = xs:
    gitignoreSource (self.lib.cleanSourceWith {
      filter = p: type: !(builtins.elem (builtins.baseNameOf p) xs);
      src = gitignoreSource ../.;
    });
in {
  ocamlPackages = self.ocaml-ng.ocamlPackages_4_07.overrideScope'
    (builtins.foldl' self.lib.composeExtensions (_: _: { }) [
      # Both opam-repository and tezos-opam-repository are updated manually with `niv update`
      (opam-nix.traverseOPAMRepo' sources.opam-repository)
      (opam-nix.traverseOPAMRepo sources.tezos-opam-repository)
      (opam-nix.callOPAMPackage (filterOut [
        ".git"
        ".gitlab-ci.yml"
        ".gitignore"
        "nix"
        "docker"
        "tools"
        "gitlab-pages"
      ]))
      (oself: osuper: {
        # Strange naming in nixpkgs
        ocamlfind = oself.findlib;
        lablgtk = null;
        lwt = oself.lwt4;

        # Native dependencies
        conf-gmp = self.gmp;
        conf-libev = self.libev;
        conf-hidapi = self.hidapi;
        conf-pkg-config = self.pkg-config;

        # Strange problems
        bigstring = osuper.bigstring.overrideAttrs (_: { doCheck = false; });
        xmldiff = osuper.xmldiff.overrideAttrs (_: { src = sources.xmldiff; });
        getopt = osuper.getopt.overrideAttrs (_: { configurePhase = "true"; });

        # Force certain versions
        ipaddr = osuper.ipaddr.versions."4.0.0";
        conduit = osuper.conduit.versions."2.1.0";
        conduit-lwt-unix = osuper.conduit-lwt-unix.versions."2.0.2";
        cohttp-lwt-unix = osuper.cohttp-lwt-unix.versions."2.4.0";
        cohttp-lwt = osuper.cohttp-lwt.versions."2.4.0";
        macaddr = osuper.macaddr.versions."4.0.0";
        ocaml-migrate-parsetree =
          osuper.ocaml-migrate-parsetree.versions."1.4.0";
        ppx_tools_versioned = osuper.ppx_tools_versioned.versions."5.2.3";
        bisect_ppx = osuper.bisect_ppx.versions."2.0.0".overrideAttrs (_: {
          src = builtins.fetchTarball
            "https://github.com/aantron/bisect_ppx/archive/02dfb10188033a26d07d23480c2bc44a3a670357.tar.gz";
        });

        proto-alpha-utils = osuper.proto-alpha-utils.overrideAttrs (oa: rec {
          buildInputs = oa.buildInputs
            ++ [ oself.tezos-protocol-006-PsCARTHA-parameters ];
          propagatedBuildInputs = buildInputs;
        });
        tezos-protocol-compiler = osuper.tezos-protocol-compiler.overrideAttrs
          (oa: rec {
            buildInputs = oa.buildInputs ++ [ oself.pprint ];
            propagatedBuildInputs = buildInputs;
          });

        # A combination of executables, libraries, documentation and test coverage
        ligo = self.buildEnv {
          name = "ligo";
          paths = with oself; [
            ligo-out.out
            ligo-tests
            ligo-doc
            ligo-coverage
          ];
        };

        # LIGO executable and public libraries
        ligo-out = osuper.ligo.overrideAttrs (oa: {
          name = "ligo-out";
          inherit CI_COMMIT_SHA COMMIT_DATE;
          buildInputs = oa.buildInputs
            ++ [ oself.UnionFind oself.Preprocessor ];
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.rakudo ];
        });

        # LIGO test suite; output empty on purpose
        ligo-tests = osuper.ligo.overrideAttrs (oa: {
          name = "ligo-tests";
          src = filterOut [
            ".git"
            ".gitlab-ci.yml"
            ".gitignore"
            "nix"
            "docker"
            "tools"
          ];
          outputs = [ "out" ];
          buildPhase = "dune runtest";
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.rakudo ];
          installPhase = "mkdir $out";
        });
        # LIGO odoc documentation
        ligo-doc = osuper.ligo.overrideAttrs (oa: {
          name = "ligo-doc";
          buildInputs = oa.buildInputs
            ++ [ oself.odoc oself.tezos-protocol-updater ];
          outputs = [ "out" ];
          buildPhase = "dune build @doc";
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.rakudo ];
          installPhase =
            "mkdir $out; cp -r _build/default/_doc/_html/ $out/doc";
        });
        # LIGO test coverage reports
        ligo-coverage = oself.ligo-tests.overrideAttrs (oa: {
          name = "ligo-coverage";
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.rakudo ];
          buildPhase = ''
            # Needed for coverage and nothing else
            mkdir -p $out/share/coverage
            echo "Coverage:"
            BISECT_ENABLE=yes dune runtest --force
            bisect-ppx-report html -o $out/share/coverage/all --title="LIGO overall test coverage"
            bisect-ppx-report summary --per-file > $out/share/coverage-all
            echo "Test coverage:"
            BISECT_ENABLE=yes dune runtest src/test --force
            bisect-ppx-report html -o $out/share/coverage/ligo --title="LIGO test coverage"
            echo "Doc coverage:"
            BISECT_ENABLE=yes dune build @doc-test --force
            bisect-ppx-report html -o $out/share/coverage/docs --title="LIGO doc coverage"
            echo "CLI test coverage:"
            BISECT_ENABLE=yes dune runtest src/bin/expect_tests
            bisect-ppx-report html -o $out/share/coverage/cli --title="CLI test coverage"
          '';
          installPhase = "true";
        });
      })
    ]);
}
