let rec internalize_typed (ds : Ast_typed.program) =
  let open Ast_typed in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = { public = false ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = M_struct (internalize_typed x)}
        | _ -> module_
      in
      Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = { public = false ; hidden = true } in
      Declaration_type { type_binder ; type_expr ; type_attr }
    | Declaration_constant x ->
      let attr : known_attributes = { x.attr with inline = true ; hidden = true } in
      Declaration_constant { x with attr }
  in
  let f (d : _ Ast_typed.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds

let rec internalize_core (ds : Ast_core.module_) =
  let open Ast_core in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = { public = false ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = M_struct (internalize_core x)}
        | _ -> module_
      in
      Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = { public = false ; hidden = true } in
      Declaration_type { type_binder ; type_expr ; type_attr }
    | Declaration_constant x ->
      let attr : known_attributes = { x.attr with inline = true ; hidden = true } in
      Declaration_constant { x with attr }
  in
  let f (d : _ Ast_core.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds


(* LanguageMap are used to cache compilation of standard libs across :
   - multiple imports (#imports)
   - multiple compilation of contract in "ligo test"
*)
module LanguageMap = Simple_utils.Map.Make(struct
  type t = Syntax_types.t * Environment.Protocols.t * bool
  let compare (sa,pa,ta) (sb,pb,tb) = Int.( abs (Syntax_types.compare sa sb) + abs (Environment.Protocols.compare pa pb) + abs (compare_bool ta tb) )
end)
type cache = (Ast_typed.module_ * Ast_core.module_) LanguageMap.t
let std_lib_cache = ref (LanguageMap.empty : cache)
let test_lib_cache = ref (LanguageMap.empty : cache)
let build_key ~options syntax = 
  let open Compiler_options in
  (syntax, options.middle_end.protocol_version, options.middle_end.test)