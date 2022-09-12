(* This pass turns 'sap_' prefixed outer type quantification into Singleton kind *)
let sap_for_all (b : (Ast_core.type_expression option) Ligo_prim.Binder.t) =
  let open Ast_core in
  let f t =
    match t.type_content with
    | T_for_all { ty_binder ; type_ ; kind = _ } when not (Ligo_prim.Type_var.is_generated ty_binder) ->
       let name = Ligo_prim.Type_var.to_name_exn ty_binder in
       if String.is_prefix name ~prefix:"sap_" then
         let type_content = T_for_all { ty_binder ; type_ ; kind = Ligo_prim.Kind.Singleton } in
         { t with type_content }
       else
         t
    | _ -> t in
  let b = Ligo_prim.Binder.map (Option.map ~f) b in
  b

(* This pass removes '@' prefix from binders.
   E.g., `let @or = ...` becomes `let or = ...` *)
let at_prefix (b : (Ast_core.type_expression option) Ligo_prim.Binder.t) =
  if not (Ligo_prim.Value_var.is_generated @@ Ligo_prim.Binder.get_var b) then
    let name = Ligo_prim.Value_var.to_name_exn @@ Ligo_prim.Binder.get_var b in
    match String.chop_prefix name ~prefix:"@" with
    | Some name -> Ligo_prim.Binder.subst_var b @@ Ligo_prim.Value_var.of_input_var name
    | None -> b
  else
    b

let internalize_core ?(inner = false) (ds : Ast_core.program) : Ast_core.program =
  let open Ast_core in
  let rec f ~inner (d : declaration_content) : declaration_content = match d with
    | D_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = TypeOrModuleAttr.{ public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = Ligo_prim.Module_expr.M_struct (module' ~inner:true x)}
        | _ -> module_
      in
      D_module { module_binder ; module_ ; module_attr }
    | D_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = TypeOrModuleAttr.{ public = false ; hidden = true } in
      D_type { type_binder ; type_expr ; type_attr }
    | D_value x ->
      let binder = sap_for_all x.binder in
      let binder = at_prefix binder in
      let attr : ValueAttr.t = { x.attr with inline = true ; hidden = true } in
      D_value { x with attr ; binder }
  and declaration ~inner (d : declaration) = Simple_utils.Location.map (f ~inner) d
  and decl ~inner d = declaration ~inner d
  and module' ~inner = List.map ~f:(decl ~inner) in

  List.map ~f:(declaration ~inner) ds

(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise : Syntax_types.t -> Ast_core.program -> Ast_core.program = fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise syntax arg in
    let attr = ValueAttr.{ inline = false ; no_mutation = true ; view = false ; public = false ; hidden = false ; thunk = false } in
    let d = Location.wrap @@ D_value { binder = make_binder (Ligo_prim.Value_var.of_input_var "cli_arg") ; expr ; attr } in
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
