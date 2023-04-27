class Ligo < Formula
  desc "Friendly Smart Contract Language for Tezos"
  homepage "https://ligolang.org/"
  license "MIT"

  # We clone repo explicitely to preserve the information about git submodules
  url "https://gitlab.com/ligolang/ligo.git", tag: "0.64.2", revision: "435a5f7e167c871916bb13b961dc97e3d2a9aec8"
  version "0.64.2"
  head "https://gitlab.com/ligolang/ligo.git", branch: "dev"


  bottle do
    root_url "https://gitlab.com/api/v4/projects/12294987/packages/generic/ligo_bottle/current"
    sha256 cellar: :any, arm64_ventura: "32ddc08549e82926e02e3419bf4d2f5573fcf628406d3dec18c16079c8b11658"
    sha256 cellar: :any, ventura: "e9266330b7910ed8ce29808d2ab97c3ad5f833c2a899b23d855b747849bf98de"
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
    ENV["LIGO_VERSION"] = "0.64.2"
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
