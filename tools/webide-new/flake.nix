{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };
  inputs = {
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    tezos-packaging.url = "github:serokell/tezos-packaging";
  };
  outputs = { self, haskell-nix, nix-npm-buildpackage, nixpkgs, flake-utils }@inputs:
  flake-utils.lib.eachSystem [ "x86_64-linux" ] (system :
    let
      pkgs = import nixpkgs {
        overlays = [ nix-npm-buildpackage.overlays.default haskell-nix.overlay ];
        localSystem = system;
      };
      ligo-binary = {
        # ligo 0.50.0
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/2959700000/artifacts/raw/ligo"; hash = "sha256-9AdoS8tUYeqdnCUSRbUxj3dZQLhk9pbEq93hFF6uSEI="; };
      };
      frontend = pkgs.callPackage ./ligo-webide-frontend/ligo-ide { };
      backend = pkgs.callPackage ./ligo-webide-backend { };
      frontendCheck = checkPhase:
        frontend.overrideAttrs (o: {
          buildInputs = o.buildInputs ++ [ pkgs.nodePackages.typescript pkgs.nodePackages.eslint ];
          buildPhase = "yarn";
          doCheck = true;
          doFixup = false;
          installPhase = "touch $out";
          inherit checkPhase;
        });
    in rec {
      packages = {
        ligo-bin = pkgs.runCommand "ligo-bin" { } ''
          install -Dm777 ${pkgs.fetchurl ligo-binary.${system}} $out/bin/ligo
        '';
        inherit frontend;
        backend = backend.components.exes.ligo-webide-backend;
      };
      tests = {
        ligo-webide-backend-test = backend.components.tests.ligo-webide-backend-test;
      };
      checks = {
        frontend-tscompile = frontendCheck "yarn run tscompile";
        frontend-tslint = frontendCheck "yarn run tslint";
      };
    }
  );
}
