# An overlay that adds ligo to ocamlPackages

{ sources ? import ./sources.nix
, CI_COMMIT_SHA ? builtins.getEnv "CI_COMMIT_SHA"
, COMMIT_DATE ? builtins.getEnv "COMMIT_DATE"
, CI_COMMIT_TAG ? builtins.getEnv "CI_COMMIT_TAG" }:
self: super:
let
  opam-nix = import sources.opam-nix (import sources.nixpkgs { });

  ocaml-overlay =
    import "${sources.tezos-packaging}/nix/build/ocaml-overlay.nix" {
      sources = import "${sources.tezos-packaging}/nix/nix/sources.nix";
    };

  fixHardeningWarning = pkg: if pkg.stdenv.isDarwin then pkg.overrideAttrs (_: {
    hardeningDisable = [ "strictoverflow" ];
  }) else pkg;

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
  ocamlPackages = (self.extend ocaml-overlay).ocamlPackages.overrideScope'
    (builtins.foldl' self.lib.composeExtensions (_: _: { }) [
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

        # Strange problems
        bigstring = osuper.bigstring.overrideAttrs (_: { doCheck = false; });
        xmldiff = osuper.xmldiff.overrideAttrs (_: { src = sources.xmldiff; });
        getopt = osuper.getopt.overrideAttrs (_: { configurePhase = "true"; });
        # Force certain versions
        ocaml-migrate-parsetree =
          osuper.ocaml-migrate-parsetree.versions."1.4.0";
        ppx_tools_versioned = osuper.ppx_tools_versioned.versions."5.2.3";
        bisect_ppx = osuper.bisect_ppx.versions."2.0.0".overrideAttrs (_: {
          src = builtins.fetchTarball
            "https://github.com/aantron/bisect_ppx/archive/02dfb10188033a26d07d23480c2bc44a3a670357.tar.gz";
        });

        hacl = fixHardeningWarning osuper.hacl;

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

        ligo-dune = osuper.ligo.override { dune = osuper.dune_2; };

        # LIGO executable and public libraries
        ligo-out = oself.ligo-dune.overrideAttrs (oa: {
          name = "ligo-out";
          LIGO_VERSION = if isNull
          (builtins.match "[0-9]+\\.[0-9]+\\.[0-9]+" CI_COMMIT_TAG) then
            (if CI_COMMIT_SHA != "" && COMMIT_DATE != "" then ''
              Rolling release
              Commit SHA: ${CI_COMMIT_SHA}
              Commit Date: ${COMMIT_DATE}
            '' else
              if builtins.elem ".git" (builtins.attrNames (builtins.readDir ../.)) then ''
                Rolling release
                Commit SHA: ${self.lib.commitIdFromGitRepo ../.git}
              '' else "Unknown: not built from a git checkout")
          else
            "${CI_COMMIT_TAG}";
          inherit CI_COMMIT_TAG CI_COMMIT_SHA COMMIT_DATE;
          CHANGELOG_PATH = builtins.toFile "changelog.txt" (builtins.readFile "${self.buildPackages.ligo-changelog}/changelog.txt");
          buildInputs = oa.buildInputs
            ++ [ oself.UnionFind oself.Preprocessor ];
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.coq ];
        });

        # LIGO test suite; output empty on purpose
        ligo-tests = oself.ligo-dune.overrideAttrs (oa: {
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
            ++ [ self.buildPackages.coq ];
          installPhase = "mkdir $out";
          buildInputs = oa.buildInputs ++ oa.checkInputs;
        });
        # LIGO odoc documentation
        ligo-doc = oself.ligo-dune.overrideAttrs (oa: {
          name = "ligo-doc";
          buildInputs = oa.buildInputs
            ++ [ oself.odoc oself.tezos-protocol-updater ];
          outputs = [ "out" ];
          buildPhase = "dune build @doc";
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.coq ];
          installPhase =
            "mkdir $out; cp -r _build/default/_doc/_html/ $out/doc";
        });
        # LIGO test coverage reports
        ligo-coverage = oself.ligo-tests.overrideAttrs (oa: {
          name = "ligo-coverage";
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ self.buildPackages.coq ];
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
  coq = (self.coq_8_12.override { buildIde = false; csdp = null; });
}
