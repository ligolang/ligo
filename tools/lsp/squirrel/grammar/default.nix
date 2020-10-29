{ stdenv, tree-sitter, nodejs }:
stdenv.mkDerivation {
  name = "ligo-grammars";
  src = ./.;

  nativeBuildInputs = [ tree-sitter nodejs ];

  HOME = "/tmp";

  installPhase = "cp -r . $out";
}
