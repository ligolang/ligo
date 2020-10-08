{ buildNpmPackage, ligo-squirrel }:
buildNpmPackage {
  src = ./.;

  npmBuild = ''
    mkdir bin
    cp ${ligo-squirrel}/bin/ligo-squirrel ./bin/ligo-squirrel
    npm run compile
    npm run package
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
