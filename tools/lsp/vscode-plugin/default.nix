{ buildNpmPackage, ligo-squirrel, haskell-nix, vscode, xvfb-run, git, writeText }:
let
  src = ../../..;
  subDir = "tools/lsp/vscode-plugin";
  codePath = "${vscode}/lib/vscode/code";
in
buildNpmPackage {
  # use cleanGit from haskell.nix
  src = haskell-nix.haskellLib.cleanGit {
    name = "vscode-plugin";
    # location relative to git root
    inherit src;
    inherit subDir;
  };

  nativeBuildInputs = [ xvfb-run git ];

  npmBuild = ''
    export HOME="$NIX_BUILD_TOP"
    export XDG_CONFIG_HOME="$NIX_BUILD_TOP"
    export XDG_RUNTIME_DIR="$NIX_BUILD_TOP"
    export CONTRACTS_DIR="$NIX_BUILD_TOP/contracts"
    cp ${../squirrel/test/contracts} "$CONTRACTS_DIR" --no-preserve=all -r
    mkdir bin
    cp -Lr ${ligo-squirrel}/* .
    npm run package
    xvfb-run npm run test -- --vscodeExecutablePath ${codePath} --extensionDevelopmentPath "$PWD" --extensionTestsPath "$PWD/client/out/test/vsc-test/index.js"
    npm run lint
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
