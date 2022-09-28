{
  description = "Nix flake provided by ligo to install bin of the last release and dependencies";

  inputs = {
    esy.url = "github:esy/esy";
  };

  outputs = { self, nixpkgs, esy }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      ligoLight = pkgs.callPackage ./nix/get_ligo_light.nix { };
      ligo = pkgs.callPackage ./nix/get_ligo.nix { esy = esy.packages.${system}.default; };
    in
    {
      packages.${system} = {
        default = ligo;
        inherit ligo ligoLight;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ ligo ];
      };
    };
}
