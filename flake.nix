{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (nixpkgs.makePkgs { inherit system; });
        michelson-of-ocaml = pkgs.callPackage ./nix/michelson-of-ocaml.nix {
          doCheck = true;
        };
      in rec {
        packages = { inherit michelson-of-ocaml; };
        devShell = import ./nix/shell.nix { inherit pkgs michelson-of-ocaml; };
      });
}
