{ buildNpmPackage, ligo-squirrel }:
buildNpmPackage {
  src = ./.;

  npmBuild = ''
    mkdir bin
    cp -Lr ${ligo-squirrel}/* .
    npm run compile
    npm run package
    npm run lint
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
