{ sources ? import ./sources.nix }@args:
let pkgs = import ./pkgs.nix args;
in {
  inherit (pkgs)
    ligo ligo-tests ligo-doc ligo-coverage
    ligo-bin ligo-static ligo-docker ligo-deb
    ligo-editor ligo-editor-docker
    ligo-website
    ligo-changelog;
}
