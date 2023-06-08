{ config, lib, pkgs, haskell-nix, stackProject, haskellLib, weeder-hacks, ... }:
let
  name = "ligo-debugger";
  ligo-bin = pkgs.runCommand "ligo-bin" {} ''
    mkdir $out
    ln -s ${../../../ligo} $out/ligo
  '';
in stackProject {
  src = haskellLib.cleanGit {
    src = ../../..;
    subDir = "tools/debugger/ligo-debugger";
    includeSiblings = true;
  };
  modules = [
    ({ ... }: {
      packages.${name} = {
        # needed for weeder
        ghcOptions = [
          "-Werror"
          "-ddump-to-file" "-ddump-hi"
        ];
        postInstall = weeder-hacks.collect-dump-hi-files;

        testWrapper = [
          (toString (pkgs.writeScript "asdf" ''
            echo 🐿  c’est n’est pas une squirrel

            export PATH=${ligo-bin}:$PATH
            echo 🐿  path maintenant: $PATH
            TEST_DIR=$TMP/test_dir
            mkdir -p $TEST_DIR
            cp -rL ${./test} $TEST_DIR
            chmod -R +w $TEST_DIR
            $@
            CODE=$?
            echo 🐿  code $CODE on $@
            echo 🐿  changes in the directory:
            diff -r $TEST_DIR ${./test}
            echo 🐿  out
            exit $CODE
          ''))
        ];
      };
    })
  ];
}
