{
  description = "Nix flake provided by ligo to install bin of the last release";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
      };
      ligo = pkgs.callPackage ./nix/get_ligo.nix { };
    in
    {
      packages.x86_64-linux.default = ligo;
    };
}
