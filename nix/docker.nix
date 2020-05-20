{ dockerTools, writeShellScriptBin, runCommand, mcpp, bash, coreutils, ligo, name ? "ligo" }:
dockerTools.buildLayeredImage {
  inherit name;
  tag = "latest";
  contents = [ ligo bash ];
  config.Entrypoint = name;
}
