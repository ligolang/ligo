{
  description = "LIGO Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/23.11";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    build-yarn-package = {
      url = "github:serokell/nix-npm-buildpackage";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # opam
    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository";
    };
    opam-nix-integration = {
      url = "github:vapourismo/opam-nix-integration";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.opam-repository.follows = "opam-repository";
    };

    # Doesn't belong here, but opam-nix-integraiton doesn't support pin-depends
    linol = {
      url = "github:c-cube/linol/7730eabf98f657059920369b41d43e657a231ed5";
      flake = false;
    };
    ocaml-recovery-parser = {
      url = "github:serokell/ocaml-recovery-parser/0.2.4";
      flake = false;
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
              opam-nix-integration.overlays.default
              (import ./nix/overlay.nix {inherit (inputs) opam-repository linol ocaml-recovery-parser tezos-ligo;})
            ];
          };

          ligo = pkgs.opamPackages.ligo;
          ligo-syntaxes = ./tools/vscode/syntaxes;
          ligo-webide = pkgs.callPackage ./nix/webide.nix {inherit ligo-syntaxes;};

          fmt = treefmt.lib.evalModule pkgs {
            projectRootFile = "dune-project";

            programs.ocamlformat.enable = true;
            programs.alejandra.enable = true;

            settings.formatter.ocamlformat.command = pkgs.lib.mkForce "${pkgs.opamPackages.ocamlformat}/bin/ocamlformat";

            settings.global.excludes = ["_build" "result" ".direnv" "vendors/*"];
          };
        in {
          packages = {
            inherit (ligo-webide) ligo-webide-backend ligo-webide-frontend;
            ligo = ligo;
            default = ligo;
          };

          devShells.default = pkgs.mkShell {
            name = "ligo-dev-shell";

            inputsFrom = [ligo];

            buildInputs = with pkgs; [
              alejandra
              shellcheck
              python3
            ];
          };

          formatter = fmt.config.build.wrapper;
        }
      );
}
