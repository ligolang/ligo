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
    haskell-nix-weeder.flake = false;
    haskell-nix.inputs.hackage.follows = "hackage";
    # This input is needed for weeder 1 compilation
    nixpkgs-legacy.url = "github:serokell/nixpkgs?rev=1714a2ead1a18678afa3cbf75dff3f024c579061";
  };

  outputs = { self, utils, haskell-nix, hackage, nixpkgs, nixpkgs-legacy, haskell-nix-weeder }:
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
        pkgs-legacy = nixpkgs-legacy.legacyPackages.${system};

        grammars = import ./ligo-debugger/grammar { pkgs = haskellPkgs; };

        weeder-hacks = import haskell-nix-weeder { pkgs = pkgs-legacy; };

        ligo-debugger-stack-project = pkgs:
          (pkgs.haskell-nix.callPackage ./ligo-debugger { inherit grammars; inherit weeder-hacks; });

        ligo-debugger-package = pkgs:
          (ligo-debugger-stack-project pkgs).ligo-debugger;

        ligo-debugger-exec = pkgs:
          (ligo-debugger-package pkgs).components.exes.ligo-debugger;
        ligo-debugger-components = ligo-debugger-package (haskellPkgs);
        ligo-debugger-test = (ligo-debugger-package haskellPkgs).components.tests.ligo-debugger-test;

        # nixpkgs has weeder 2, but we use weeder 1 because `weeder-hacks`
        # still don't support it.
        weeder-legacy = pkgs-legacy.haskellPackages.callHackageDirect {
          pkg = "weeder";
          ver = "1.0.9";
          sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
        } {};

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
            weeder-script = weeder-hacks.weeder-script {
              weeder = weeder-legacy;
              hs-pkgs = ligo-debugger-stack-project haskellPkgs;
              local-packages = [
                { name = "ligo-debugger"; subdirectory = "ligo-debugger"; }
              ];
            };

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
          };
        };
      in archOut);
}
