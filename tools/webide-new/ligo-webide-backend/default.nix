{ haskell-nix, runCommand }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.cleanSourceHaskell {
      src = ./.;
      name = "ligo-webide-backend";
    };
  };
  exes = project.ligo-webide-backend.components.exes;
  ligo-webide-backend = project.ligo-webide-backend;
  swagger-gen = exes.swagger-gen;
  swagger-file = runCommand "swagger.json" {
    LANG = "C.UTF-8";
    buildInputs = [ swagger-gen ];
  } "mkdir -p $out; swagger-gen > $out/swagger.json";
in { inherit
       swagger-file ligo-webide-backend; }
