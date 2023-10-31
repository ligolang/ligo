class LigoAT110 < Formula
  desc "Friendly Smart Contract Language for Tezos"
  homepage "https://ligolang.org/"
  license "MIT"

  # We clone repo explicitely to preserve the information about git submodules
  url "https://gitlab.com/ligolang/ligo.git", tag: "1.1.0", revision: "edcf7f67c70016705068934d29dfcd531f6908f0"
  version "1.1.0"
  head "https://gitlab.com/ligolang/ligo.git", branch: "dev"


  bottle do
    root_url "https://gitlab.com/api/v4/projects/12294987/packages/generic/ligo_bottle/current"
  # bottle arm64_ventura
  # bottle ventura
  end

  build_dependencies = %w[opam rust hidapi pkg-config gnu-sed cmake gcc]
  build_dependencies.each do |dependency|
    depends_on dependency => :build
  end

  dependencies = %w[gmp libev libffi]
  dependencies.each do |dependency|
    depends_on dependency
  end

  # sets up env vars for opam before running a command
  private def with_opam_env(cmd)
    "eval \"$(opam config env)\" && #{cmd}"
  end

  def install
    # ligo version is taken from the environment variable in build-time
    ENV["LIGO_VERSION"] = "1.1.0"
    # avoid opam prompts
    ENV["OPAMYES"] = "true"

    # init opam state in ~/.opam
    system "opam", "init", "--bare", "--auto-setup", "--disable-sandboxing"
    # create opam switch with required ocaml version
    system "scripts/setup_switch.sh"
    # TODO: remowe workarounds below and use the script provided by the ligo repo once
    # a new version is released
    # Required for Tezos hangzhou protocol
    system "git", "submodule", "init"
    system "git", "submodule", "update", "--recursive"
    # Because sed has different options on MacOS >:(
    system "gsed -i 's/sed/gsed/g' scripts/install_vendors_deps.sh"
    # Build dependencies
    system with_opam_env "scripts/install_vendors_deps.sh"
    # build ligo
    system with_opam_env "dune build -p ligo"

    # install ligo binary
    cp "_build/install/default/bin/ligo", "ligo"
    bin.mkpath
    bin.install "ligo"
  end

  test do
    system "#{bin}/ligo", "--help"
  end
end
