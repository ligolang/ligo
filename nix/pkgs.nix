{ sources ? import ./sources.nix }:
let
  ocaml-overlay = import ./ocaml-overlay.nix { inherit sources; };
  static-overlay = import ./static-overlay.nix pkgs;
  mac-overlay = import ./mac-overlay.nix;
  nodejs-overlay = import ./nodejs-overlay.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ ocaml-overlay nodejs-overlay ]
      ++ (if builtins.currentSystem == "x86_64-darwin"
          then [ mac-overlay ]
          else [ ]);
  };
  separateBinary = pkg:
    pkgs.runCommandNoCC "${pkg.name}-bin" { }
    "mkdir -p $out/bin; cp -Lr ${pkg}/ligo $out/bin";

  nix-npm-buildpackage = pkgs.callPackage sources.nix-npm-buildpackage { };
in pkgs.extend (self: super: {
  inherit (self.ocamlPackages) ligo ligo-out ligo-tests ligo-doc ligo-coverage;
  ligo-bin = separateBinary self.ligo-out.bin;
  ligo-docker = self.callPackage ./docker.nix { ligo = self.ligo-bin; };
  ligo-deb = self.callPackage ./packageDeb.nix { };
  ligo-editor = self.callPackage ./ligo-editor.nix { inherit sources; };
  ligo-editor-docker = self.callPackage ./docker.nix {
    ligo = self.ligo-editor;
    name = "ligo-editor";
  };
  ligo-website = self.callPackage ./ligo-website.nix {
    inherit (nix-npm-buildpackage) buildNpmPackage;
  };
  ligo-static = self.pkgsMusl.ligo-bin;
  pkgsMusl = super.pkgsMusl.extend static-overlay;
})
