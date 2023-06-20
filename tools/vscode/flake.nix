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

        vscode-extension = (pkgs.callPackage ./default.nix {}).extension;
      in {
        packages = {
          inherit vscode-extension;
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
