{ runCommand }:
let
  src = ./.;
in
runCommand "ligo-syntaxes" {} ''
  mkdir $out
  cp -rL ${src}/. $out
''
