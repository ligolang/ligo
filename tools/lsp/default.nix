let
  sources = import ./nix/sources.nix;
  ligo-squirrel = import ./squirrel { };
  pkgs = import sources.nixpkgs { };
  nix-npm-buildpackage = pkgs.callPackage sources.nix-npm-buildpackage { };
in {
  ligo-vscode-extension = pkgs.callPackage ./vscode-plugin { inherit ligo-squirrel; inherit (nix-npm-buildpackage) buildNpmPackage; };
}
