let internalize_typed ?(inner = false) (ds : Ast_typed.program) : Ast_typed.program =
  let open Ast_typed in
  let rec f ~inner (d : declaration_content) : declaration_content = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = Attr.{ public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = Ligo_prim.Module_expr.M_struct (module' ~inner:true x)}
        | _ -> module_
      in
      Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = Attr.{ public = false ; hidden = true } in
      Declaration_type { type_binder ; type_expr ; type_attr }
    | Declaration_constant x ->
      let attr : Attr.value = { x.attr with inline = true ; hidden = true } in
      Declaration_constant { x with attr }
  and declaration ~inner (d : declaration) = Simple_utils.Location.map (f ~inner) d
  and decl ~inner (Decl d) = Decl (declaration ~inner d)
  and module' ~inner = List.map ~f:(decl ~inner) in

  List.map ~f:(declaration ~inner) ds

let internalize_core ?(inner = false) (ds : Ast_core.program) : Ast_core.program =
  let open Ast_core in
  let rec f ~inner (d : declaration_content) : declaration_content = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = Attr.{ public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = Ligo_prim.Module_expr.M_struct (module' ~inner:true x)}
        | _ -> module_
      in
      Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = Attr.{ public = false ; hidden = true } in
      Declaration_type { type_binder ; type_expr ; type_attr }
    | Declaration_constant x ->
      let attr : Attr.value = { x.attr with inline = true ; hidden = true } in
      Declaration_constant { x with attr }
  and declaration ~inner (d : declaration) = Simple_utils.Location.map (f ~inner) d
  and decl ~inner (Decl d) = Decl (declaration ~inner d)
  and module' ~inner = List.map ~f:(decl ~inner) in

  List.map ~f:(declaration ~inner) ds

(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise : Syntax_types.t -> Ast_core.program -> Ast_core.program = fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise syntax arg in
    let attr = Attr.{ inline = false ; no_mutation = true ; view = false ; public = false ; hidden = false ; thunk = false } in
    let d = Location.wrap @@ Declaration.Declaration_constant { binder = make_binder (Ligo_prim.ValueVar.of_input_var "cli_arg") ; expr ; attr } in
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
type cache = (Ast_typed.program * Ast_core.program) LanguageMap.t
let std_lib_cache = ref (LanguageMap.empty : cache)
let build_key ~options syntax =
  let open Compiler_options in
  (syntax, options.middle_end.protocol_version, options.middle_end.test)
