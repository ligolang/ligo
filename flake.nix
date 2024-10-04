{
  description = "LIGO Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    build-yarn-package = {
      url = "github:serokell/nix-npm-buildpackage";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Also doesn't belong here, but required to avoid nix's bad UX with submodules
    tezos-ligo = {
      url = "gitlab:ligolang/tezos-ligo/v21-ligo";
      flake = false;
    };

    grace = {
      url = "github:johnyob/grace";
      flake = false;
    };

    lltz = {
      url = "github:trilitech/lltz";
      flake = false;
    };
  };
  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (
        system: let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              build-yarn-package.overlays.default
              ocaml-overlay.overlays.default
              (import ./nix/overlay.nix)
              (_: prev:
                with prev; {
                  ocamlPackages = ocaml-ng.ocamlPackages_4_14;
                  coqPackages = coqPackages_8_13;
                  ocamlformat = ocaml-ng.ocamlPackages_4_14.ocamlformat_0_21_0;
                })
            ];
          };

          tree-sitter-typescript = pkgs.callPackage ./nix/tree-sitter-typescript.nix {};
          ligo = pkgs.callPackage ./nix/ligo.nix {inherit tezos-ligo tree-sitter-typescript grace lltz;};
          ligo-syntaxes = ./tools/vscode/syntaxes;

          fmt = treefmt.lib.evalModule pkgs {
            projectRootFile = "dune-project";

            programs.ocamlformat.enable = true;
            programs.alejandra.enable = true;

            settings.global.excludes = ["_build" "result" ".direnv" "vendors/*" "vendored-dune/*"];
          };

          ligo-shell = slow: {
              name = "ligo-dev-shell";

              inputsFrom = [ligo];

              buildInputs = with pkgs; [
                alejandra
                ocamlformat
                ocamlPackages.utop
                ocamlPackages.ocaml-lsp
                ocamlPackages.merlin
                ocamlPackages.merlin-lib
              ] ++ lib.optionals slow [
                shellcheck
                emacsPackages.merlin
                emacsPackages.merlin-company
              ];

              shellHook = ''
                # This is a hack to work around the hack used in the dune files
                export TREE_SITTER="${ligo.TREE_SITTER}";
                export TREE_SITTER_TYPESCRIPT="${ligo.TREE_SITTER_TYPESCRIPT}";
              '';
            };
        in {
          packages = {
            ligo = ligo;
            default = ligo;
          };

          devShells = rec {
            default = pkgs.mkShell (ligo-shell false);
            slow = pkgs.mkShell (ligo-shell true);
          };

          formatter = fmt.config.build.wrapper;
        }
      );
}
