{
  nixConfig = {
    extra-substituters = [
      "https://hydra.iohk.io\?want-mass-query=1"
    ];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  };

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    ligo = {
      url = "git+https://gitlab.com/serokell/ligo/ligo";
      flake = false;
    };
  };

  outputs = { nixpkgs, haskell-nix, self, ... }:
    let
      ligo-binary = {
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/2385579945/artifacts/raw/ligo"; hash = "sha256-UFGPJTPxyyKvlcY8Lh3poY6anOtyErCUTfZi2BdkBM4="; };
      };

      platforms = [ "x86_64-linux" ];
      filteredPlatforms = with builtins; listToAttrs (map (f: { name = f; value = nixpkgs.legacyPackages.${f}; }) platforms);
      onPkgs = f: with builtins; mapAttrs f filteredPlatforms;
      sources = builtins.path { path = ./.; filter = (path: type: (builtins.match "(.*\\.nix|.*\\.lock)" path) == null); };
    in
    {
      nixosModules.default = { config, pkgs, lib, ... }:
        let system = pkgs.system; in
        {

          options = with pkgs.lib; {

            services.ligo-webide-frontend = {
              enable = mkEnableOption "ligo-webide service";

              serverName = mkOption {
                type = types.str;
                default = "localhost";
                description = ''
                  Name of the nginx virtualhost to use.
                '';
              };

              listenHost = mkOption {
                type = types.str;
                default = "localhost";
                description = ''
                  Listen address for the virtualhost to use.
                '';
              };

            };

            services.ligo-webide = {
              enable = mkEnableOption "ligo-webide service";
            };
          };

          config = with pkgs.lib; let
            cfg_frontend = config.services.ligo-webide-frontend;
            cfg = config.services.ligo-webide;
            req = self.packages.${system};
          in
          lib.mkIf cfg.enable {
            systemd.services.ligo-webide = {
              after = [ "network.target" ];
              wantedBy = [ "multi-user.target" ];
              script =
                ''
                  ${req.backend}/bin/ligo-webide-backend --ligo-path ${req.ligo-bin}/bin/ligo
                '';

            };


            services.nginx = {
              enable = true;
              # recommendedProxySettings = true;
              virtualHosts.ligo-webide = {
                serverName = cfg_frontend.serverName;
                root = req.webide;
                locations."/" = {
                  index = "index.html";
                  tryFiles = "$uri $uri/ /index.html =404";
                };
                locations."~ ^/local(?<route>/static/.*)" = {
                  alias = req.webide + "$route";
                };
                locations."~ ^/api(?<route>/.*)" = {
                  proxyPass = "http://127.0.0.1:8080$route";
                };
              };
            };

            networking.firewall.allowedTCPPorts = [ 80 443 ];

          };
        };

      packages = onPkgs (system: pkgs:
        with pkgs;
        let
          a_haskell-nix = haskell-nix.legacyPackages.${system}.haskell-nix;
          y2n = pkgs.yarn2nix-moretea;
        in
        rec {

          ligo-bin = pkgs.runCommand "ligo-bin" { } ''
            install -Dm777 ${pkgs.fetchurl ligo-binary.${system}} $out/bin/ligo
          '';

          backend =
            let
              name = "ligo-webide-backend";
              proj = a_haskell-nix.stackProject {
                src = a_haskell-nix.cleanSourceHaskell {
                  src = ./ligo-webide-backend;
                  inherit name;
                };
              };
            in
            proj.${name}.components.exes.ligo-webide-backend;

          webide = y2n.mkYarnPackage {
                src = ./ligo-webide-frontend/ligo-ide;
                buildPhase = ''
                  # moving all of the node_modules into a new, mutable folder,
                  # because some weird npm (react-app-rewire iirc) thing decided that it needs node_modules/.cache

                  pushd "$PWD/deps/ligo-ide"
                    cp -raL "node_modules" "node_modules.mut"
                    chmod -R +rw "node_modules.mut"

                    # bin needs to stay symlinked
                    rm -rf node_modules.mut/.bin
                    cp -r "node_modules/.bin" "node_modules.mut/.bin"

                    rm "node_modules"
                    mv "node_modules.mut" "node_modules"
                  popd

                  # gotta love node's non-existent module isolation
                  # hack in highlight.js from global deps
                  cp -r node_modules/highlight.js "$PWD/deps/ligo-ide/node_modules"

                  yarn --offline --frozen-lockfile build:react
                '';
                distPhase = ":";
                installPhase = ''
                  cp -rL $PWD/deps/ligo-ide/build $out
                '';
              };

        }

      );

    };

}
