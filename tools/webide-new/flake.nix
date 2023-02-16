{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };
  inputs = {
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    tezos-packaging.url = "github:serokell/tezos-packaging";
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage = {
      flake = false;
    };
    stackage = {
      flake = false;
    };
  };
  outputs = { self, haskell-nix, nix-npm-buildpackage, nixpkgs, flake-utils, tezos-packaging, deploy-rs, ... }@inputs:
  {
    nixosModules.default = { config, pkgs, lib, ... }:
      let system = pkgs.system; in
      with pkgs.lib; {
        options.services.ligo-webide-frontend = {
          enable = mkEnableOption "ligo-webide service";

          package = mkOption {
            type = types.path;
            default = self.packages.x86_64-linux.frontend;
          };

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
          package = mkOption {
            type = types.path;
            default = self.packages.x86_64-linux.backend;
          };
          ligo-package = mkOption {
            type = types.path;
            default = self.packages.x86_64-linux.ligo-bin;
          };
          tezos-client-package = mkOption {
            type = types.path;
            default = self.packages.x86_64-linux.tezos-client;
          };
          gist-token = mkOption {
            type = types.path;
            description = "Path to the file with token that is used to manage GitHub gists.";
          };
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
            # Don't attempt to start
            unitConfig.ConditionPathExists = [ webide-cfg.package webide-cfg.ligo-package webide-cfg.tezos-client-package ];
            script =
              ''
                ${webide-cfg.package}/bin/ligo-webide-backend \
                  --ligo-path ${webide-cfg.ligo-package}/bin/ligo \
                  --octez-client-path ${webide-cfg.tezos-client-package}/bin/octez-client \
                  --gist-token "$(cat ${webide-cfg.gist-token})"
              '';

          };

          services.nginx = {
            enable = true;
            # recommendedProxySettings = true;
            virtualHosts.ligo-webide = {
              serverName = frontend-cfg.serverName;
              root = frontend-cfg.package;
              locations."/" = {
                index = "index.html";
                tryFiles = "$uri $uri/ /index.html =404";
              };
              locations."~ ^/(local|share)(?<route>/static/.*)" = {
                alias = frontend-cfg.package + "$route";
              };
              locations."~ ^/api(?<route>/.*)" = {
                proxyPass = "http://127.0.0.1:8080$route";
              };
            };
          };

          networking.firewall.allowedTCPPorts = [ 80 443 ];

        };
      };
    deploy = let
      webide-profile = (import nixpkgs { localSystem = "x86_64-linux"; }).linkFarm "webide-profile" [
        { name = "backend"; path = self.packages.x86_64-linux.backend; }
        { name = "frontend"; path = self.packages.x86_64-linux.frontend; }
        { name = "tezos-client"; path = self.packages.x86_64-linux.tezos-client; }
        { name = "ligo"; path = self.packages.x86_64-linux.ligo-bin; }
      ];
    in {
      sshOpts = [ "-p 17788" ];
      nodes.webide = {
        # TODO: perhaps it should be moved to a dedicated server
        hostname = "tejat-prior.gemini.serokell.team";
        user = "deploy";
        profiles = {
          # restart backend service and check that front page returns 200
          webide.path = deploy-rs.lib.x86_64-linux.activate.custom
            webide-profile ''sudo /run/current-system/sw/bin/nixos-container run ligo-webide-thing -- bash -c \
              'systemctl restart ligo-webide.service; curl --silent --show-error --fail http://127.0.0.1:80 > /dev/null'
            '';
        };
      };
    };
  } // (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system :
    let
      haskellPkgs = haskell-nix.legacyPackages."${system}";
      pkgs = import nixpkgs {
        overlays = [ nix-npm-buildpackage.overlays.default haskell-nix.overlay ];
        localSystem = system;
      };
      ligo-binary = {
        # ligo 86aca37be2fc4fc46efbd74e47d7991f6800ca8e
        # To update the used ligo version, find corresponding 'docker_extract' job in CI pipeline for
        # the given revision and use its id below. To get the hash, use 'nix-prefetch-url <url>'
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/3779146953/artifacts/raw/ligo"; sha256 = "00did3dlfscsw221swidbs9kkklzfkg74kyfd4hr6qqcs9mmjrr1"; };
      };
      ligo-syntaxes = pkgs.callPackage ../lsp/vscode-plugin/syntaxes {};
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
        ligo-bin = pkgs.runCommand "ligo-bin" { } ''
          install -Dm777 ${pkgs.fetchurl ligo-binary.${system}} $out/bin/ligo
        '';
        inherit tezos-client swagger-file;
        frontend = frontend.package;
        backend = backend.ligo-webide-backend.components.exes.ligo-webide-backend;
        openapi-client = frontend.openapi-client swagger-file;
      };
      tests = {
        ligo-webide-backend-test = backend.ligo-webide-backend.components.tests.ligo-webide-backend-test;
      };
      checks = {
        frontend-tscompile = frontendCheck "yarn run tscompile";
        frontend-tslint = frontendCheck "yarn run tslint";
        frontend-openapi = pkgs.runCommand "frontend-api-check" {} ''
          diff -q ${backend-generated-openapi} ${frontend.src}/src/components/api/generated
          touch $out
        '';
      } // deploy-rs.lib.${system}.deployChecks self.deploy;
      devShell = pkgs.mkShell {
        buildInputs = [
          deploy-rs.defaultPackage.${system}
        ];
      };
    }
  ));
}
