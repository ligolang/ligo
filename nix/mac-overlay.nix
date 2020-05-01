oself: osuper:
let
  fixHardeningWarning = pkg: pkg.overrideAttrs (_: {
    hardeningDisable = [ "strictoverflow" ];
  });
in
{
  hacl = fixHardeningWarning osuper.hacl;
}
