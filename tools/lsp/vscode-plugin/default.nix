{ lib
, yarn2nix-moretea
, ligo-squirrel
, python3
, vscodium
, xvfb-run
, git
, zip
, unzip
, findutils
, writeText
, pkg-config
, libsecret
, doRunTests ? true
}:
let
  codePath = "${vscodium}/lib/vscode/codium";
in
yarn2nix-moretea.mkYarnPackage {
  src = ./.;

  patchPhase = ''
    export CONTRACTS_DIR="$NIX_BUILD_TOP/contracts"
    cp --remove-destination ${../../../LICENSE.md} ./LICENSE.md
    cp ${../squirrel/test/contracts} "$CONTRACTS_DIR" --no-preserve=all -r
    cp -Lr ${ligo-squirrel}/* .
  '';

  preBuild = ''
    # broken link fest
    rm deps/$pname/$pname
  '';

  postBuild = ''
    yarn run package
    yarn run lint

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
