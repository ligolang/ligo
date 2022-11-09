{ buildYarnPackage, python3, pkg-config, libsecret }:

buildYarnPackage {
  src = ./.;
  integreties = builtins.fromJSON (builtins.readFile ./integreties.json);
  buildInputs = [ python3 pkg-config libsecret ];
  yarnBuildMore = "yarn --offline build:react";
  installPhase = ''
    cp -rL $PWD/build $out
  '';
}
