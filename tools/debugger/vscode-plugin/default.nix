{ lib, yarn2nix-moretea, python3, vscodium, xvfb-run, git, zip, unzip, findutils
, writeText, pkg-config, doRunTests ? false, doCopyBinary ? true, ligo-debugger }:
let codePath = "${vscodium}/lib/vscode/codium";
in yarn2nix-moretea.mkYarnPackage {
  src = ./.;

  preBuild = ''
    # broken link fest
    rm deps/$pname/$pname

    # fixing license path
    rm deps/$pname/LICENSE.md
    cp -L ${../../..}/tools/debugger/vscode-plugin/LICENSE.md deps/$pname/LICENSE.md

    mkdir -p deps/$pname/bin
    cp -Lr ${ligo-debugger}/bin/. deps/$pname/bin
  '';

  postBuild = ''
    yarn run package

    ${lib.optionalString doRunTests ''
      echo 🛠 🧪 Running VSCode tests ...
      export HOME=$NIX_BUILD_TOP
      pushd deps/$pname
      ${xvfb-run}/bin/xvfb-run \
        yarn run test -- \
          --vscodeExecutablePath ${codePath} \
          --extensionDevelopmentPath "$PWD" \
          --extensionTestsPath "$PWD/client/out/test/vsc-test/index.js"
      popd
    ''}

    # r13y!
    VSIX=$PWD/deps/$pname/*.vsix
    mkdir extracted
    ${unzip}/bin/unzip $VSIX -d extracted

    # clean all of the update times in the archive
    ${findutils}/bin/find extracted -exec touch -d @$SOURCE_DATE_EPOCH '{}' ';'

    pushd extracted
    # strip extended attributes and creation time
    ${zip}/bin/zip -X --latest-time -r $VSIX *
    popd

  '';

  installPhase = ''
    mkdir $out
    mv deps/$pname/*.vsix $out
  '';

  distPhase = ":";

}
