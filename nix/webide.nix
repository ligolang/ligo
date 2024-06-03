{
  haskell-nix,
  runCommand,
  buildYarnPackage,
  libsecret,
  pkg-config,
  python3,
  openapi-generator-cli,
  ligo-syntaxes,
  ...
} @ pkgs: let
  # WebIDE backend
  ligo-webide-backend-project = haskell-nix.stackProject {
    src = haskell-nix.cleanSourceHaskell {
      src = ../tools/webide-new/ligo-webide-backend;
      name = "ligo-webide-backend";
    };
  };
  ligo-webide-backend-components = ligo-webide-backend-project.ligo-webide-backend.components;

  ligo-webide-backend = ligo-webide-backend-components.exes.ligo-webide-backend // {passthru.project = ligo-webide-backend-project;};
  ligo-webide-openapi-file =
    runCommand "swagger.json" {
      LANG = "C.UTF-8";
      buildInputs = [ligo-webide-backend-components.exes.swagger-gen];
    } "mkdir -p $out; swagger-gen > $out/swagger.json"
    // {meta.artifacts = ["/swagger.json"];};

  # WebIDE frontend
  ligo-webide-frontend = buildYarnPackage rec {
    # FIXME: Remove ligo-ide folder
    src = ../tools/webide-new/ligo-webide-frontend/ligo-ide;

    NODE_OPTIONS = "--openssl-legacy-provider";

    # some files in ./src/ligo-components/ligo-project/Project/languages/syntaxes are symlinks
    # from some adjacent file tree, so they have to be copied explicitly
    postUnpack = ''
      cp --remove-destination ${ligo-syntaxes}/*.json \
        $sourceRoot/src/ligo-components/ligo-project/Project/languages/syntaxes
    '';

    integreties = builtins.fromJSON (builtins.readFile "${src}/integreties.json");

    buildInputs = [python3 pkg-config libsecret];

    yarnBuildMore = "yarn --offline build:react-prod";

    installPhase = ''
      cp -rL $PWD/build $out
    '';
  };

  ligo-webide-openapi-client =
    runCommand "ligo-webide-openapi-client" {
      buildInputs = [openapi-generator-cli];
    } ''
      openapi-generator-cli generate \
        -i ${ligo-webide-openapi-file}/swagger.json \
        -g typescript-fetch \
        -o $out
    '';
in {
  inherit
    ligo-webide-openapi-file
    ligo-webide-backend
    ligo-webide-frontend
    ligo-webide-openapi-client
    ;
}
