{ haskell-nix }:

(haskell-nix.stackProject {
  src = haskell-nix.cleanSourceHaskell {
    src = ./.;
    name = "ligo-webide-backend";
  };
}).ligo-webide-backend
