module ModuleWithPrivate =
  Gitlab_pages.Docs.Advanced.Src.Attributes_decorators.Module_with_private

(* foo = 5167 = (123 * 42) + 1 *)
let foo = ModuleWithPrivate.f 123

(*
  The following lines cause errors because g and stuff are private:

  let bad_1 = ModuleWithPrivate.g 123
  let bad_2 = ModuleWithPrivate.stuff
*)