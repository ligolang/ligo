{ buildNpmPackage, ligo-squirrel }:
buildNpmPackage {
  src = ./.;

  npmBuild = ''
    mkdir bin
    cp -Lr ${ligo-squirrel}/* .
    npm run compile
    npm run package
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
