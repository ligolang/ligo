#import "gitlab-pages/docs/tezos/decorators/src/module-with-private.mligo" "ModuleWithPrivate"

let foo = ModuleWithPrivate.f 123  // = 5167

(*
  The following lines cause errors because g and stuff are private:

  let bad_1 = ModuleWithPrivate.g 123
  let bad_2 = ModuleWithPrivate.stuff
*)