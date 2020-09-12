{ stdenv, tree-sitter, nodejs }:
stdenv.mkDerivation {
  name = "ligo-grammars";
  src = ./.;

  nativeBuildInputs = [ tree-sitter nodejs ];

  HOME = "/tmp";

  installPhase = ''
    for i in */src/parser.c; do
      langname=$(dirname $(dirname $i))
      mkdir -p $out/$langname
      cp $i $out/$langname/parser.c
    done
  '';
}
