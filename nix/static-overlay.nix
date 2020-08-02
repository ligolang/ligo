# An overlay that adds flags needed to build LIGO statically;
# Supposed to be applied to pkgsMusl
# Takes `native` as a package set that doesn't cause mass rebuilds (so that we don't have to build perl with musl)
native: self: super:
let dds = x: x.overrideAttrs (o: { dontDisableStatic = true; });
in {
  buildPackages = super.buildPackages // { inherit (native) rakudo upx ligo-changelog; };
  ocaml = self.ocaml-ng.ocamlPackages_4_09.ocaml;
  libev = dds super.libev;
  libusb = self.libusb1;
  systemd = self.eudev;
  libusb1 = dds (super.libusb1.override { enableSystemd = true; });
  gdb = null;
  hidapi = dds (super.hidapi.override { systemd = self.eudev; });
  glib = (super.glib.override { libselinux = null; }).overrideAttrs
    (o: { mesonFlags = o.mesonFlags ++ [ "-Dselinux=disabled" ]; });
  eudev = dds (super.eudev.overrideAttrs
    (o: { nativeBuildInputs = o.nativeBuildInputs ++ [ super.gperf ]; }));
  gmp = dds (super.gmp);
  ocamlPackages = super.ocamlPackages.overrideScope' (self: super: {
    ligo-out =
      super.ligo-out.overrideAttrs (_: { patches = [ ./static.patch ]; });
  });
}
