{

  nixConfig = {
    flake-registry =
      "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
    extra-substituters = [ "https://hydra.iohk.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    utils.url = "github:numtide/flake-utils";
    hackage.flake = false;
    haskell-nix.inputs.hackage.follows = "hackage";
  };

  outputs = { self, utils, haskell-nix, hackage, nixpkgs }:
    let haskellSystems = (builtins.attrNames haskell-nix.legacyPackages);
    in utils.lib.eachSystem haskellSystems (system:
      let
        haskellPkgs = haskell-nix.legacyPackages."${system}";
        nixpkgsArgs = {
          overlays = [(self: super: {
            # Hash for one of headers is broken in nixpkgs at the moment :(
            musl = super.musl.override (_: { useBSDCompatHeaders = false; });
          })];
          config = { allowUnfree = true; };
          localSystem = system;
        };

        pkgs = import nixpkgs nixpkgsArgs;

        grammars = import ./ligo-debugger/grammar { pkgs = haskellPkgs; };

        ligo-debugger-package = pkgs:
          (pkgs.haskell-nix.callPackage ./ligo-debugger { inherit grammars; }).ligo-debugger;

        ligo-debugger-exec = pkgs:
          (ligo-debugger-package pkgs).components.exes.ligo-debugger;
        ligo-debugger-components = ligo-debugger-package (haskellPkgs);
        ligo-debugger-test = (ligo-debugger-package haskellPkgs).components.tests.ligo-debugger-test;
        lsp-test = (ligo-debugger-package haskellPkgs).components.tests.lsp-test;
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

            ligo-debugger = ligo-debugger-exec (haskellPkgs);
            ligo-debugger-static = ligo-debugger-exec (haskellPkgs.pkgsCross.musl64);

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
            inherit lsp-test;
          };
        };
      in archOut);
}
