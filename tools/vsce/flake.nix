{

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  outputs =
    { self, haskell-nix, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        vsce = import ./vsce.nix { inherit pkgs; };
      in {
        packages = { inherit vsce; };
        checks = {
          vsce-test = pkgs.stdenv.mkDerivation {
            name = "vsce-test";
            buildInputs = [ vsce ];
            outputs = [ "out" ];
            phases = [ "checkPhase" ];
            doCheck = true;
            checkPhase = ''
              vsce --help
              ovsx --help
              touch $out
            '';
          };
        };
        devShell = pkgs.mkShell {
          buildInputs = [ vsce ];
        };
      }
    );
}
