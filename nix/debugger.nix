{haskell-nix, ...} @ pkgs: let
  ligo-debugger-project = haskell-nix.stackProject {
    src = haskell-nix.cleanSourceHaskell {
      src = ../tools/debugger/ligo-debugger;
      name = "ligo-debugger";
    };
  };

  ligo-debugger-components = ligo-debugger-project.ligo-debugger.components;

  ligo-debugger = ligo-debugger-components.exes.ligo-debugger // {passthru.project = ligo-debugger-project;};
in
  ligo-debugger
