{ buildNpmPackage, squirrel }:
buildNpmPackage {
  src = ./.;

  npmBuild = ''
    mkdir bin
    cp ${squirrel}/bin/squirrel ./bin/squirrel
    npm run compile
    npm run package
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
