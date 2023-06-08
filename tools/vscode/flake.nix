{

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
    extra-substituters = [ "https://cache.iohk.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    utils.url = "github:numtide/flake-utils";
    hackage.flake = false;
    haskell-nix.inputs.hackage.follows = "hackage";
    haskell-nix-weeder.flake = false;
    nixpkgs-legacy.url = "github:serokell/nixpkgs?rev=1714a2ead1a18678afa3cbf75dff3f024c579061";
  };

  outputs =
    { self, utils, haskell-nix, hackage, nixpkgs, nixpkgs-legacy, haskell-nix-weeder }@inputs:
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

        weeder-hacks = import haskell-nix-weeder { pkgs = pkgs-legacy; };

        ligo-debugger-stack-project = pkgs:
          (pkgs.haskell-nix.callPackage ./../debugger/ligo-debugger { inherit weeder-hacks; });

        ligo-debugger-package = pkgs:
          (ligo-debugger-stack-project pkgs).ligo-debugger;

        ligo-debugger-exec = pkgs:
          (ligo-debugger-package pkgs).components.exes.ligo-debugger;

        ligo-debugger-static = ligo-debugger-exec (haskellPkgs.pkgsCross.musl64);
        # darwin ligo-debugger binary with all .dylib dependencies bundled
        ligo-debugger-darwin = self.packages.x86_64-darwin.ligo-debugger.overrideAttrs (_: {
          # otherwise, binary is linked with libraries from /nix/store
          postInstall = ''
            mkdir -p $out/lib
            ${../scripts/relink-mac-binary.sh} $out/bin/ligo-debugger ../lib
          '';
        });
        ligo-debugger-crossplatform-dispatcher = pkgs.writeTextFile {
          name = "ligo-debugger";
          text = ''
            #!/usr/bin/env bash
            "$(dirname "''${BASH_SOURCE[0]}")/$(uname)/bin/ligo-debugger" $@
          '';
          executable = true;
        };
        ligo-debugger-crossplatform = pkgs.linkFarm "ligo-debugger-crossplatform" [
          { name = "bin/ligo-debugger"; path = ligo-debugger-crossplatform-dispatcher; }
          { name = "bin/Linux"; path = ligo-debugger-static; }
          { name = "bin/Darwin"; path = ligo-debugger-darwin; }
        ];

        vscode-extension-native = (pkgs.callPackage ./default.nix {
          ligo-debugger = ligo-debugger-exec (haskellPkgs);
        }).extension;

        vscode-extension = (pkgs.callPackage ./default.nix {
          ligo-debugger = ligo-debugger-crossplatform;
        }).extension;
      in {
        packages = {
          inherit vscode-extension;
          inherit vscode-extension-native;
          ligo-debugger = ligo-debugger-exec (haskellPkgs);
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
