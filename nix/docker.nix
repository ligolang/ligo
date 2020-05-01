{ dockerTools, writeShellScriptBin, runCommand, mcpp, bash, coreutils, ligo, name ? "ligo" }:
let
  tmp = runCommand "tmp" {} "mkdir -p $out/tmp";
in
dockerTools.buildLayeredImage {
  inherit name;
  tag = "latest";
  contents = [ ligo tmp bash ];
  config.Entrypoint = name;
}
