{

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  outputs =
    { self, flake-utils, nixpkgs  }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        nixpkgsArgs = {
          overlays = [];
          config = { allowUnfree = true; };
          localSystem = system;
        };

        pkgs = import nixpkgs nixpkgsArgs;

        ligo-bin = pkgs.linkFarm "ligo-bin" [ {
          name = "bin/ligo";
          path = "${../../ligo}";
        } ];

        vscode-extension-project = pkgs.callPackage ./default.nix {};
        vscode-extension = vscode-extension-project.extension;
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
              vscode-extension-package
            ];
            pathsToLink = [ "/libexec" ];
          };
          config = {
            Cmd = [
              "xvfb-run" "yarn" "run" "test" "--"
                  "--vscodeExecutablePath" "/lib/vscode/codium"
                  "--extensionDevelopmentPath" "/libexec/ligo-vscode/deps/ligo-vscode"
                  "--extensionTestsPath" "/libexec/ligo-vscode/deps/ligo-vscode/client/out/test/vsc-test/index.js"
            ];
            Env = [ "CONTRACTS_DIR=/libexec/ligo-vscode/deps/ligo-vscode/test/contracts" ];
            WorkingDir = "/libexec/ligo-vscode/deps/ligo-vscode";
          };
          extraCommands = ''
            mkdir -m 0777 ./tmp
          '';
        };
      in {
        packages = {
          inherit vscode-extension;
        };
        # Tests for vs-code extension are impure and cannot be run as a nix derivation build
        tests = {
          # VSCode extensions tests are impure and cannot be easily run
          # as a part of nix derivation building, so we're running them
          # inside docker container instead
          inherit vscode-extenstion-test-docker-image;
        };
        defaultPackage = self.packages.${system}.vscode-extension;
        # For debug/development reasons only
        legacyPackages = pkgs;
        devShells = {
          default = pkgs.mkShell rec {
            buildInputs = [ pkgs.nodejs ];
          };
        };
      });
}
