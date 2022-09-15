{
  description = "Nix flake provided by ligo to install bin of the last release";

  inputs = {
    esy.url = "github:esy/esy";
  };

  outputs = { self, nixpkgs, esy }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      ligo = pkgs.callPackage ./nix/get_ligo.nix { };
      withEsy = pkgs.callPackage ./nix/with_esy.nix { esy = esy.packages.${system}.default; };
    in
    {
      packages.${system} = {
        default = ligo;
        inherit ligo withEsy;
      };
    };
}
