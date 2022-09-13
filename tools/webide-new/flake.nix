{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };
  inputs = {
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    tezos-packaging.url = "github:serokell/tezos-packaging";
  };
  outputs = { self, haskell-nix, nix-npm-buildpackage, nixpkgs, flake-utils, tezos-packaging }@inputs:
  {
    nixosModules.default = { config, pkgs, lib, ... }:
      let system = pkgs.system; in
      with pkgs.lib; {
        options.services.ligo-webide-frontend = {
          enable = mkEnableOption "ligo-webide service";

          serverName = mkOption {
            type = types.str;
            default = "localhost";
            description = ''
              Name of the nginx virtualhost to use.
            '';
          };
        };

        options.services.ligo-webide = {
          enable = mkEnableOption "ligo-webide service";
        };

        config = with pkgs.lib; let
          frontend-cfg = config.services.ligo-webide-frontend;
          webide-cfg = config.services.ligo-webide;
          packages = self.packages.${system};
        in
        lib.mkIf webide-cfg.enable {
          systemd.services.ligo-webide = {
            after = [ "network.target" ];
            wantedBy = [ "multi-user.target" ];
            script =
              ''
                ${packages.backend}/bin/ligo-webide-backend --ligo-path ${packages.ligo-bin}/bin/ligo --tezos-client-path ${packages.tezos-client}/bin/tezos-client
              '';

          };

          services.nginx = {
            enable = true;
            # recommendedProxySettings = true;
            virtualHosts.ligo-webide = {
              serverName = frontend-cfg.serverName;
              root = packages.frontend;
              locations."/" = {
                index = "index.html";
                tryFiles = "$uri $uri/ /index.html =404";
              };
              locations."~ ^/local(?<route>/static/.*)" = {
                alias = packages.frontend + "$route";
              };
              locations."~ ^/api(?<route>/.*)" = {
                proxyPass = "http://127.0.0.1:8080$route";
              };
            };
          };

          networking.firewall.allowedTCPPorts = [ 80 443 ];

        };
      };
  } // (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system :
    let
      pkgs = import nixpkgs {
        overlays = [ nix-npm-buildpackage.overlays.default haskell-nix.overlay ];
        localSystem = system;
      };
      ligo-binary = {
        # ligo 0.50.0
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/2959700000/artifacts/raw/ligo"; hash = "sha256-9AdoS8tUYeqdnCUSRbUxj3dZQLhk9pbEq93hFF6uSEI="; };
      };
      tezos-client = inputs.tezos-packaging.packages.${system}.tezos-client;
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
        inherit frontend tezos-client;
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
  ));
}
