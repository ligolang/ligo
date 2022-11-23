{ buildYarnPackage, fetchurl, python3, openapi-generator-cli, runCommand, pkg-config, libsecret }:
let
  src = ./.;
  package = buildYarnPackage {
    inherit src;
    integreties = builtins.fromJSON (builtins.readFile ./integreties.json);
    buildInputs = [ python3 pkg-config libsecret ];
    yarnBuildMore = "yarn --offline build:react";
    installPhase = ''
      cp -rL $PWD/build $out
    '';
  };

  openapi-generator-cli-6 = openapi-generator-cli.overrideAttrs (oa: rec {
      version = "6.2.0";
      jarfilename = "${oa.pname}-${version}.jar";
      src = fetchurl {
        url =
          "mirror://maven/org/openapitools/${oa.pname}/${version}/${jarfilename}";
        sha256 = "sha256-YHB+LIk4qUJ49iFggdcGfQ8b7O2MjrEnfmJemlnM0to=";
      };
  });
  openapi-client = swagger-file: runCommand "openapi-client" {
      buildInputs = [ openapi-generator-cli-6 ];
    } ''
      openapi-generator-cli generate \
        -i ${swagger-file}/swagger.json \
        -g typescript-axios \
        -o $out
      '';
in { inherit package openapi-client src; }
