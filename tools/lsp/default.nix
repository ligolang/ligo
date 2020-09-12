let
  sources = import ./nix/sources.nix;
  squirrel = import ./squirrel { };
  pkgs = import sources.nixpkgs { };
  nix-npm-buildpackage = pkgs.callPackage sources.nix-npm-buildpackage { };
in {
  ligo-vscode-extension = pkgs.callPackage ./vscode-plugin { inherit squirrel; inherit (nix-npm-buildpackage) buildNpmPackage; };
}
