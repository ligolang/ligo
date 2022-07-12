{

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
#    extra-substituters = "ssh-ng://bunda.aquarius.serokell.team";
  };

  outputs =
    { self, haskell-nix, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
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
                 dialects = ["camligo" "reasonligo" "pascaligo" "jsligo"];
             in pkgs.lib.strings.concatStrings (map testDialect dialects)
                + "touch $out";
        };

        # n.b.: If the dependency on ligo is changed for any test, remember to
        # also update the main functions of the respective tests.
        integration-test = squirrel.checks.integration-test.overrideAttrs (oldAttrs: {
          buildInputs = [ ligo-bin ] ++ oldAttrs.buildInputs;
        });

        lsp-handlers-test = squirrel.checks.lsp-handlers-test.overrideAttrs (oldAttrs: {
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
          pkg.overrideAttrs (_: {
            postInstall = with pkgs; ''
              mkdir -p $out/lib
              cp ${gmp}/lib/* $out/lib
              chmod -R 777 $out/lib/
              install_name_tool -change ${gmp}/lib/libgmp.10.dylib @executable_path/../lib/libgmp.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${libffi}/lib/libffi.8.dylib /usr/lib/libffi.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${libiconv}/lib/libiconv.dylib /usr/lib/libiconv.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${darwin.Libsystem}/lib/libSystem.B.dylib /usr/lib/libSystem.B.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${darwin.Libsystem}/lib/libSystem.B.dylib /usr/lib/libSystem.B.dylib $out/lib/libgmp.dylib
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
        vscode-extension-native = pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = exes.squirrel-static;
        };

        # Multiarch vscode ext
        vscode-extension = pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = ligo-squirrel-combined;
        };
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
          # LSP binary tests
          inherit integration-test;
          # yeah
          inherit lint;
        };
        defaultPackage = self.packages.${system}.vscode-extension-native;
        # For debug/development reasons only
        legacyPackages = pkgs;
        devShell = pkgs.mkShell rec {
          buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
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
