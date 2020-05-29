

let
  sources = import ./nix/sources.nix { };
  nixpkgs = import sources.nixpkgs { };
  init-el = nixpkgs.writeText "init.el" ''
    (eval-when-compile
      ${
        builtins.concatStringsSep "\n    "
        (builtins.map (package: ''(add-to-list 'load-path "${package}")'')
          (with sources; [ use-package tuareg company-mode nix-mode ]))
      }
      (require 'use-package)
    )

    (use-package company)

    (use-package nix-mode
      :mode "\\.nix\\'")

    (load "${sources.tuareg}/tuareg-site-file")

    ${builtins.readFile ./ocaml-config.el}

    (setq inhibit-startup-message t)

    (message "Welcome to LIGO Instant Emacs")
  '';
  emacs = nixpkgs.runCommand "emacs" {
    nativeBuildInputs = [ nixpkgs.makeWrapper ];
  } ''
    makeWrapper ${nixpkgs.emacs}/bin/emacs $out/bin/emacs \
      --add-flags "-l ${init-el}" \
      --add-flags "-q"
  '';

in nixpkgs.mkShell {
  buildInputs = [ sources.niv emacs nixpkgs.lorri nixpkgs.tmux nixpkgs.direnv ];
  shellHook = ''eval "$(${nixpkgs.direnv}/bin/direnv hook bash)"'';
}
