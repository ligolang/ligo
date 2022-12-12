{

  nixConfig = {
    flake-registry =
      "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
    extra-substituters = [ "https://hydra.iohk.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = { utils.url = "github:numtide/flake-utils"; };

  outputs = { self, utils, haskell-nix }:
    let haskellSystems = (builtins.attrNames haskell-nix.legacyPackages);
    in utils.lib.eachSystem haskellSystems (system:
      let
        pkgs = haskell-nix.legacyPackages.${system};

        grammars = import ./../lsp/squirrel/grammar { inherit pkgs; };

        ligo-debugger-package = pkgs:
          (pkgs.haskell-nix.callPackage ./ligo-debugger { inherit grammars; }).ligo-debugger;

        ligo-debugger-exec = pkgs:
          (ligo-debugger-package pkgs).components.exes.ligo-debugger;
        ligo-debugger-components = ligo-debugger-package (pkgs);
        ligo-debugger-test = (ligo-debugger-package pkgs).components.tests.ligo-debugger-test;
        archOut = {
          devShells = {
            default = pkgs.mkShell rec {
              buildInputs = [ pkgs.nixfmt ];
            };
            ci = pkgs.mkShell {
              buildInputs = [ pkgs.danger-gitlab pkgs.haskellPackages.hlint ];
            };
          };

          packages = {

            ligo-debugger = ligo-debugger-exec (pkgs);
            ligo-debugger-static = ligo-debugger-exec (pkgs.pkgsCross.musl64);

            ligo-debugger-extension = pkgs.callPackage ./vscode-plugin {
              # Since we are shipping it, we want to have a portable binary
              ligo-debugger = archOut.packages.ligo-debugger-static;
            };

            ligo-debugger-extension-nix = pkgs.callPackage ./vscode-plugin {
              # Well, this one is the nix one, we don't make it portable.
              ligo-debugger = archOut.packages.ligo-debugger;
            };
          };

          inherit ligo-debugger-components;
          tests = {
            inherit ligo-debugger-test;
          };
        };
      in archOut);
}
