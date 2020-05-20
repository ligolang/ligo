{ dockerTools, writeShellScriptBin, runCommand, mcpp, bash, coreutils, ligo, name ? "ligo" }:
let
  # LIGO requires /tmp for compilation, which is missing in the default image
  tmp = runCommand "tmp" {} "mkdir -p $out/tmp";
in
dockerTools.buildLayeredImage {
  inherit name;
  tag = "latest";
  contents = [ ligo tmp bash ];
  config.Entrypoint = name;
}
