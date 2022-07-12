{ sources ? import ./sources.nix }@args:
let
  pkgs = import (sources.nixpkgs) { };
  inherit (pkgs.yarn2nix-moretea) mkYarnModules;
  buildNodeJs = pkgs.callPackage "${sources.nixpkgs-upstream}/pkgs/development/web/nodejs/nodejs.nix" { };
  modules = mkYarnModules {
    yarnLock = ./vsix-packaging/yarn.lock;
    packageJSON = ./vsix-packaging/package.json;
    version = "0";
    pname = "packaging";
    yarnFlags = [
      "--offline"
      "--frozen-lockfile"
      "--ignore-platform"
      "--ignore-scripts"
      "--no-progress"
      "--non-interactive"
      "--disable-pnp"
    ];
  };
in pkgs.runCommand "packaging" {}  ''
      cp -Lr ${modules} $out
      chmod +rw -R $out
      mkdir $out/bin

      # ovsx is a very special windows cookie
      substituteInPlace $out/node_modules/ovsx/lib/ovsx \
        --replace "#!/usr/bin/env node" $"#!${pkgs.nodejs}/bin/node
        "

      ln -s $out/node_modules/ovsx/lib/ovsx $out/bin/ovsx
      ln -s $out/node_modules/vsce/vsce $out/bin/vsce
  ''
