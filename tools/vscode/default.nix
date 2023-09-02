{ lib
, yarn2nix-moretea
, zip
, unzip
, findutils
, ligo-debugger
}:
let
  package = yarn2nix-moretea.mkYarnPackage {
    src = ./.;

    patchPhase = ''
      cp --remove-destination ${../../LICENSE.md} ./LICENSE.md
    '';

    preBuild = ''
      # broken link fest
      rm deps/$pname/$pname

      mkdir -p deps/$pname/bin
      cp -Lr ${ligo-debugger}/bin/. deps/$pname/bin
    '';

    postBuild = ''
      yarn run package
      # TODO fix eslint warnings
      # yarn run lint
    '';
    distPhase = ":";
  };
in
{
  inherit package;
  extension = package.overrideAttrs(o: {
    postBuild = o.postBuild + ''
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
  });
}
