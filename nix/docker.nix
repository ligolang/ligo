{ dockerTools, writeShellScriptBin, runCommand, mcpp, bash, coreutils, ligo
, name ? "ligo", extraContents ? [ ] }:
dockerTools.buildLayeredImage {
  inherit name;
  tag = "latest";
  contents = [ ligo bash ] ++ extraContents;
  config.Entrypoint = name;
}
