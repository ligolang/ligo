{
  inputs = {
    haskell-nix.url =
      "github:input-output-hk/haskell.nix/bd45da822d2dccdbb3f65d0b52dd2a91fd65ca4e";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    nixpkgs.url = "github:serokell/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, haskell-nix, nix-npm-buildpackage, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import haskell-nix.sources.nixpkgs {
          overlays = [
            nix-npm-buildpackage.overlay
            haskell-nix.overlay
            (final: prev:
              let
                tree-sitter-prebuilt-tarballs = {
                  x86_64-linux = final.fetchurl {
                    url =
                      "https://github.com/tree-sitter/tree-sitter/releases/download/0.16.9/tree-sitter-linux-x64.gz";
                    sha256 =
                      "0apcs1gg1p3b3psbk36ssvrfzvqrmhfqz7ajca1w6mdqn66ri9nc";
                  };
                  x86_64-darwin = final.fetchurl {
                    url =
                      "https://github.com/tree-sitter/tree-sitter/releases/download/0.16.9/tree-sitter-osx-x64.gz";
                    sha256 =
                      "19zpl9lzqyqgsbsfk16y4sigpp46g3i3xpz3k5hmzfdr2ryvb4wr";
                  };
                };
                tree-sitter-prebuilt = builtins.mapAttrs (system: tarball:
                  final.stdenv.mkDerivation {
                    name = "tree-sitter-prebuilt";
                    src = tarball;
                    phases = [ "unpackPhase" "fixupPhase" ];
                    unpackPhase =
                      "mkdir -p $out/bin; zcat $src > $out/bin/tree-sitter; chmod +x $out/bin/tree-sitter";
                    fixupPhase =
                      final.lib.optionalString (system != "x86_64-darwin")
                      "patchelf --set-interpreter ${final.stdenv.glibc}/lib/ld-linux-x86-64.so.2 $out/bin/tree-sitter";
                  }) tree-sitter-prebuilt-tarballs;
              in { tree-sitter = tree-sitter-prebuilt.${system}; })
            (_: _: {
              inherit grammars;
            }) # We don't want any overlays (static, cross, etc) applied to grammars
          ];
          localSystem = system;
        };

        grammars = pkgs.callPackage ./squirrel/grammar { };

        squirrel = pkgs.callPackage ./squirrel { };

        squirrel-static = pkgs.pkgsCross.musl64.callPackage ./squirrel { };

        squirrel-windows = pkgs.pkgsCross.mingwW64.callPackage ./squirrel { };

        exes =
          builtins.mapAttrs (_: project: project.components.exes.ligo-squirrel)
          ({
            inherit squirrel;
          } // (if system != "x86_64-darwin" then {
            inherit squirrel-static squirrel-windows;
          } else
            { }));

        vscode-extension = pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = exes.squirrel-static or exes.squirrel;
        };
      in {
        packages = exes // { inherit vscode-extension; };
        defaultPackage = self.packages.${system}.vscode-extension;
        # For debug/development reasons only
        legacyPackages = pkgs;
      });
}
