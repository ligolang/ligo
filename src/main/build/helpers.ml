let rec internalize_typed ?(inner = false) (ds : Ast_typed.program) =
  let open Ast_typed in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = { public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = M_struct (internalize_typed ~inner:true x)}
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

let rec internalize_core ?(inner = false) (ds : Ast_core.module_) =
  let open Ast_core in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = { public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = M_struct (internalize_core ~inner:true x)}
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

(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise : Syntax_types.t -> Ast_core.module_ -> Ast_core.module_ = fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise syntax arg in
    let attr = { inline = false ; no_mutation = true ; view = false ; public = false ; hidden = false ; force_inline = false } in
    let d = Location.wrap @@ Declaration_constant { binder = make_binder (ValueVar.of_input_var "cli_arg") ; expr ; attr } in
    d::prg
  in
  (Option.value_map Compiler_options.(options.test_framework.cli_expr_inj) ~default:prg ~f:inject_arg_declaration)


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
let build_key ~options syntax =
  let open Compiler_options in
  (syntax, options.middle_end.protocol_version, options.middle_end.test)
