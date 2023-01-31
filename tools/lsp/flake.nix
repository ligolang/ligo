{

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
#    extra-substituters = "ssh-ng://bunda.aquarius.serokell.team";
  };

  inputs = {
    hackage.flake = false;
    haskell-nix.inputs.hackage.follows = "hackage";
  };

  outputs =
    { self, haskell-nix, flake-utils, nixpkgs, hackage  }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        haskellPkgs = haskell-nix.legacyPackages."${system}";
        nixpkgsArgs = {
          overlays = [];
          config = { allowUnfree = true; };
          localSystem = system;
        };

        pkgs = import nixpkgs nixpkgsArgs;

        # Grammar for the LSP
        grammars = import ./squirrel/grammar { pkgs = haskellPkgs; };

        # Language server itself
        squirrel = haskellPkgs.callPackage ./squirrel {
            inherit grammars;
        };

        ligo-bin = pkgs.linkFarm "ligo-bin" [ {
          name = "bin/ligo";
          path = "${../../ligo}";
        } ];

        squirrel-sexp-test = haskellPkgs.stdenv.mkDerivation {
          name = "squirrel-sexp-test";
          src = ./squirrel;
          buildInputs = [ pkgs.bats squirrel.components.exes.ligo-vet ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase = ''
            bats ./scripts
            touch $out
          '';
        };

        squirrel-grammar-test = haskellPkgs.stdenv.mkDerivation {
          name = "squirrel-grammar-test";
          HOME = "/tmp";
          src = "${grammars}";
          buildInputs = [ pkgs.tree-sitter ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase =
            let testDialect = dialect: ''
                   cd ${dialect}
                   tree-sitter test
                   cd ..
                 '';
                 dialects = ["camligo" "jsligo"];
             in pkgs.lib.strings.concatStrings (map testDialect dialects)
                + "touch $out";
        };

        # n.b.: If the dependency on ligo is changed for any test, remember to
        # also update the main functions of the respective tests.
        integration-test = squirrel.components.tests.integration-test;

        lsp-handlers-test = squirrel.checks.lsp-handlers-test.overrideAttrs (oldAttrs: {
          # 'ligo' binary that is used in these tests need ca-certificates in runtime
          SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          buildInputs = [ ligo-bin self.packages.x86_64-linux.squirrel-static ] ++ oldAttrs.buildInputs;
        });

        lint = pkgs.stdenv.mkDerivation {
          name = "lint";
          src = ./squirrel;
          buildInputs = [ pkgs.haskellPackages.hlint ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase = ''
            bash scripts/lint.sh
            touch $out
          '';
        };

        pack = pkg:
          pkg.overrideAttrs (_:  let relink-script = ../scripts/relink-mac-binary.sh; in {
            postInstall = with pkgs; ''
              mkdir -p $out/lib
              ${relink-script} $out/bin/ligo-squirrel ../lib
            '';
          });

        # LSP, static version
        squirrel-static = if system == "x86_64-darwin" then {
          components.exes.ligo-squirrel =
            pack squirrel.components.exes.ligo-squirrel;
        } else
          haskellPkgs.pkgsCross.musl64.callPackage ./squirrel {
            # Use standard build for hpack because it's available in nix binary cache
            inherit (pkgs) hpack;
            inherit grammars;
          };

        exes = builtins.mapAttrs
          (_: project: project.components.exes.ligo-squirrel) {
            inherit squirrel squirrel-static;
          };

        per-platform-dispatcher = pkgs.writeTextFile {
          name = "ligo-squirrel";
          text = ''
            #!/bin/sh
            "./bin/$(uname)/bin/ligo-squirrel" "$@"
          '';
          executable = true;
        };

        # all the LSPs for all of the platforms
        ligo-squirrel-combined = pkgs.linkFarm "ligo-squirrel-combined" [
          {
            name = "bin/ligo-squirrel";
            path = per-platform-dispatcher;
          }
          {
            name = "bin/Linux";
            path = self.packages.x86_64-linux.squirrel-static;
          }
          {
            name = "bin/Darwin";
            path = self.packages.x86_64-darwin.squirrel-static;
          }
        ];

        # Single-arch vscode ext
        vscode-extension-project = (pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = exes.squirrel-static;
        });
        vscode-extension-native = vscode-extension-project.extension;
        vscode-extension-package = vscode-extension-project.package;

        vscode-extenstion-test-docker-image = with pkgs.dockerTools; buildImage {
          name = "vscode-extension-test";
          tag = "latest";
          fromImage = buildImage {
            name = "vscode-extension-test-base";
            tag = "latest";
            copyToRoot = pkgs.buildEnv {
              name = "vscode-extension-env-base";
              paths = with pkgs; [
                vscodium ligo-bin nodePackages.esy yarn nodejs xvfb-run
                bashInteractive caCertificates coreutils curl perl gnutar gzip
              ];
              pathsToLink = [ "/bin" "/lib" "/etc" ];
            };
          };
          copyToRoot = pkgs.buildEnv {
            name = "vscode-extension-env";
            paths = [
              vscode-extension-package ./squirrel/test
            ];
            pathsToLink = [ "/libexec" "/contracts" ];
          };
          config = {
            Cmd = [
              "xvfb-run" "yarn" "run" "test" "--"
                  "--vscodeExecutablePath" "/lib/vscode/codium"
                  "--extensionDevelopmentPath" "/libexec/ligo-vscode/deps/ligo-vscode"
                  "--extensionTestsPath" "/libexec/ligo-vscode/deps/ligo-vscode/client/out/test/vsc-test/index.js"
            ];
            Env = [ "CONTRACTS_DIR=/contracts" ];
            WorkingDir = "/libexec/ligo-vscode/deps/ligo-vscode";
          };
          extraCommands = ''
            mkdir -m 0777 ./tmp
          '';
        };

        # Multiarch vscode ext
        vscode-extension = (pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = ligo-squirrel-combined;
        }).extension;

        stack2cabal = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.stack2cabal
          (drv: { jailbreak = true; broken = false; });
      in {
        packages = exes // {
          inherit vscode-extension-native vscode-extension;
        };
        checks = {
          # Prints AST in S-expression format, and checks it
          inherit squirrel-sexp-test;
          # tree-sitter tests
          inherit squirrel-grammar-test;
          # Checks library component of language-server
          inherit (squirrel.checks) lsp-test;
          # Library parsing tests
          inherit (squirrel.checks) ligo-contracts-test;
          # Runs LSP and checks its methods
          inherit lsp-handlers-test;
          # yeah
          inherit lint;
        };
        # Some of the tests are impure and cannot be run as a nix derivation build
        tests = {
          # LSP binary tests
          inherit integration-test;
          # VSCode extensions tests are impure and cannot be easily run
          # as a part of nix derivation building, so we're running them
          # inside docker container instead
          inherit vscode-extenstion-test-docker-image;
        };
        defaultPackage = self.packages.${system}.vscode-extension-native;
        # For debug/development reasons only
        legacyPackages = pkgs;
        devShells = {
          default = pkgs.mkShell rec {
            buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
          };
          ci = pkgs.mkShell {
            buildInputs = [ pkgs.stylish-haskell pkgs.gnumake stack2cabal ];
          };
          integration-test = pkgs.mkShell {
            buildInputs = [ ligo-bin pkgs.nodePackages.esy ];
          };
        };

      }) // rec {

        lsp-docker-image-default = lsp-docker-image {};

        lsp-docker-image = { creationDate ? "1970-01-01T00:00:01Z" }: self.legacyPackages.x86_64-linux.dockerTools.buildImage {
          name = "ligo-lsp";
          tag = "latest";
          created = creationDate;
          contents = self.packages.x86_64-linux.squirrel-static;
          config = {
            Entrypoint = [ "ligo-squirrel" ];
          };

          # language server needs /tmp directory
          extraCommands = ''
            mkdir -m 0777 ./tmp
          '';
        };

        # skopeo package used by CI
        skopeo = self.legacyPackages.x86_64-linux.skopeo;
      };
}
