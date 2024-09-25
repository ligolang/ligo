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

    # haskell
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Also doesn't belong here, but required to avoid nix's bad UX with submodules
    tezos-ligo = {
      url = "gitlab:ligolang/tezos-ligo/v20.0-rc1-ligo";
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
              haskell-nix.overlay
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
          ligo = pkgs.callPackage ./nix/ligo.nix {inherit tezos-ligo tree-sitter-typescript;};
          ligo-syntaxes = ./tools/vscode/syntaxes;
          ligo-webide = pkgs.callPackage ./nix/webide.nix {inherit ligo-syntaxes;};
          ligo-debugger = pkgs.callPackage ./nix/debugger.nix {};

          fmt = treefmt.lib.evalModule pkgs {
            projectRootFile = "dune-project";

            programs.ocamlformat.enable = true;
            programs.alejandra.enable = true;

            settings.global.excludes = ["_build" "result" ".direnv" "vendors/*" "vendored-dune/*"];
          };

          # Wrap stack to work with our haskell.nix integration.
          # - no-nix: we don't want stack's nix integration
          # --system-ghc: use the existing GHC on PATH (provided by haskell.nix)
          # --no-install-ghc : don't try to install GHC if no matching GHC found on PATH
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [pkgs.stack];
            buildInputs = [pkgs.makeWrapper];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };

          haskellShell = name: drv:
            drv.passthru.project.shellFor {
              inherit name;

              # FIXME: https://github.com/input-output-hk/haskell.nix/issues/1885
              # Adding [exactDeps = true] to avoid the build failure, this ensures
              # that cabal doesn't choose alternate plans, so that *all* dependencies
              # are provided by nix.
              exactDeps = true;

              buildInputs = [stack-wrapped];
            };
        in {
          packages = {
            inherit (ligo-webide) ligo-webide-backend ligo-webide-frontend;
            inherit ligo-debugger;
            ligo = ligo;
            default = ligo;
          };

          devShells = with ligo-webide; rec {
            default = pkgs.mkShell {
              name = "ligo-dev-shell";

              inputsFrom = [ligo];

              buildInputs = with pkgs; [
                alejandra
                shellcheck
                ocamlformat
                ocamlPackages.utop
                ocamlPackages.ocaml-lsp
                ocamlPackages.merlin
                ocamlPackages.merlin-lib
                emacsPackages.merlin
                emacsPackages.merlin-company
              ];

              shellHook = ''
                # This is a hack to work around the hack used in the dune files
                export OPAM_SWITCH_PREFIX="${ligo.OPAM_SWITCH_PREFIX}";
                export TREE_SITTER="${ligo.TREE_SITTER}";
                export TREE_SITTER_TYPESCRIPT="${ligo.TREE_SITTER_TYPESCRIPT}";
              '';
            };

            webide-frontend = pkgs.mkShell {
              name = "ligo-webide-frontend-shell";

              inputsFrom = [ligo-webide-frontend];

              NODE_OPTIONS = "--openssl-legacy-provider";
            };

            webide-backend = haskellShell "ligo-webide-backend-shell" ligo-webide-backend;

            webide = pkgs.mkShell {
              name = "ligo-webide-shell";

              inputsFrom = [webide-frontend webide-backend];

              NODE_OPTIONS = "--openssl-legacy-provider";
            };

            debugger = haskellShell "ligo-debugger-shell" ligo-debugger;
          };

          formatter = fmt.config.build.wrapper;
        }
      );
}
