{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    tezos-packaging.url = "github:serokell/tezos-packaging";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
  };
  outputs = { self, haskell-nix, nix-npm-buildpackage, nixpkgs, flake-utils, tezos-packaging, ... }@inputs:
  (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system :
    let
      haskellPkgs = haskell-nix.legacyPackages."${system}";
      pkgs = import nixpkgs {
        overlays = [ nix-npm-buildpackage.overlays.default haskell-nix.overlay ];
        localSystem = system;
      };
      ligo-binary = {
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/4687472710/artifacts/raw/ligo"; hash = "sha256-wwxc2Sncq1ojcdVLv1FbFxc4FHbz2t9Fw3oJPCKMVSI="; };
      };
      ligo-syntaxes = ../vscode/syntaxes;
      tezos-client = inputs.tezos-packaging.packages.${system}.tezos-client;
      frontend = (pkgs.callPackage ./ligo-webide-frontend/ligo-ide { inherit ligo-syntaxes; }) { git-proxy = "https://ligo-webide-cors-proxy.serokell.team"; };
      backend = haskellPkgs.callPackage ./ligo-webide-backend { };
      swagger-file = backend.swagger-file // {
        meta.artifacts = [ "/swagger.json" ];
      };
      backend-generated-openapi = frontend.openapi-client swagger-file;
      frontendCheck = checkPhase:
        frontend.package.overrideAttrs (o: {
          buildInputs = o.buildInputs ++ [ pkgs.nodePackages.typescript pkgs.nodePackages.eslint ];
          buildPhase = "yarn";
          doCheck = true;
          doFixup = false;
          installPhase = "touch $out";
          inherit checkPhase;
        });
    in rec {
      packages = {
        inherit swagger-file;
        frontend = frontend.package;
        backend = backend.ligo-webide-backend.components.exes.ligo-webide-backend;
        openapi-client = frontend.openapi-client swagger-file;
      } // (pkgs.lib.optionalAttrs (system == "x86_64-linux") { # prebuilt ligo binary and nix-based octez-client are only available for x86_64-linux
        ligo-bin = pkgs.runCommand "ligo-bin" { } ''
          install -Dm777 ${pkgs.fetchurl ligo-binary.${system}} $out/bin/ligo
        '';
        inherit tezos-client;
      });
      tests = {
        ligo-webide-backend-test = backend.ligo-webide-backend.components.tests.ligo-webide-backend-test;
      };
      checks = {
        frontend-tscompile = frontendCheck "yarn run tscompile";
        frontend-tslint = frontendCheck "yarn run tslint";
        frontend-openapi = pkgs.runCommand "frontend-api-check" {} ''
          diff ${backend-generated-openapi} ${frontend.src}/src/components/api/generated
          touch $out
        '';
      };
    }
  ));
}
