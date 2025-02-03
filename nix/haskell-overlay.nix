final: prev:
with prev; {
  libsodium = libsodium.overrideAttrs (with libsodium; rec {
    version = "1.0.18";
    src = final.fetchurl {
      url = "https://download.libsodium.org/libsodium/releases/${pname}-${version}.tar.gz";
      hash = "sha256-b1BEkLNCpPikxKAvybhmy++GItXfTlRStGvhIeRmNsE=";
    };
  });
}
