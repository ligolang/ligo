(* This is gitlab-pages/docs/advanced/src/attributes-decorators/import-module-with-private.mligo *)
#import "gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.mligo" "ModuleWithPrivate"

(* foo = 5167 = (123 * 42) + 1 *)
let foo = ModuleWithPrivate.f 123

(*
  The following lines cause errors because g and stuff are private:

  let bad_1 = ModuleWithPrivate.g 123
  let bad_2 = ModuleWithPrivate.stuff
*)