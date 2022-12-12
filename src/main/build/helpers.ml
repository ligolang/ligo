module Location = Simple_utils.Location
open Ligo_prim

let loc = Location.env

(* This pass turns 'sap_' prefixed outer type quantification into Singleton kind *)
let sap_for_all (b : Ast_core.type_expression option Binder.t) =
  let open Ast_core in
  let f t =
    match t.type_content with
    | T_for_all { ty_binder; type_; kind = _ } when not (Type_var.is_generated ty_binder)
      ->
      let name = Type_var.to_name_exn ty_binder in
      if String.is_prefix name ~prefix:"sap_"
      then (
        let type_content = T_for_all { ty_binder; type_; kind = Kind.Singleton } in
        { t with type_content })
      else t
    | _ -> t
  in
  let b = Binder.map (Option.map ~f) b in
  b


(* This pass removes '@' prefix from binders.
   E.g., `let @or = ...` becomes `let or = ...` *)
let at_prefix (b : Ast_core.type_expression option Binder.t) =
  if not (Value_var.is_generated @@ Binder.get_var b)
  then (
    let var = Binder.get_var b in
    let loc = Value_var.get_location var in
    let name = Value_var.to_name_exn var in
    match String.chop_prefix name ~prefix:"@" with
    | Some name -> Binder.set_var b @@ Value_var.of_input_var ~loc name
    | None -> b)
  else b


let internalize_core (ds : Ast_core.program) : Ast_core.program =
  let open Ast_core in
  let rec f (d : declaration_content) : declaration_content =
    match d with
    | D_module { module_binder; module_; module_attr } ->
      let module_ =
        match module_ with
        | { wrap_content = M_struct x; _ } ->
          { module_ with wrap_content = Module_expr.M_struct (module' x) }
        | _ -> module_
      in
      D_module { module_binder; module_; module_attr }
    | D_type { type_binder; type_expr; type_attr } ->
      D_type { type_binder; type_expr; type_attr }
    | D_irrefutable_match x -> D_irrefutable_match x
    | D_value x ->
      let binder = sap_for_all x.binder in
      let binder = at_prefix binder in
      let attr : ValueAttr.t = { x.attr with inline = true; hidden = true } in
      D_value { x with attr; binder }
  and declaration (d : declaration) = Simple_utils.Location.map f d
  and decl d = declaration d
  and module' = List.map ~f:decl in
  List.map ~f:declaration ds


(* [get_aliases_prelude] [var] [typed_program] returns aliases for all non-private declarations (module , type and value) in [typed_program].
   Declarations are accessed accessed through [var].

  e.g. with [var] == Curry
    ```
    module A = <..>
    module B = struct
      type baz = <..>
      module C = <..>
    end
    [@private] module D = <..>
    type bar = <..>
    let foo = <..>
    ```
  we return
    ```
    module A = Curry.A
    module B = Curry.B
    let foo = Curry.foo
    type bar = <..>
    ```
*)
let get_aliases_prelude
    : Ast_typed.module_variable -> Ast_typed.program -> Ast_core.program
  =
 fun mod_binder prg ->
  let get_mod_bindings acc d =
    let open Ast_typed in
    match Location.unwrap d with
    | D_module { module_binder; module_attr; _ }
      when TypeOrModuleAttr.(module_attr.public) -> module_binder :: acc
    | D_type _ | D_module _ | D_value _ | D_irrefutable_match _ -> acc
  in
  let get_val_bindings acc d =
    let open Ast_typed in
    match Location.unwrap d with
    | D_value { binder; attr; _ } when ValueAttr.(attr.public) -> binder :: acc
    | D_irrefutable_match { pattern; attr; _ } when ValueAttr.(attr.public) ->
      let binders = Pattern.binders pattern in
      binders @ acc
    | D_type _ | D_module _ | D_value _ | D_irrefutable_match _ -> acc
  in
  let get_ty_bindings acc d =
    let open Ast_typed in
    match Location.unwrap d with
    | D_type { type_binder; type_attr; _ } when type_attr.public -> type_binder :: acc
    | D_type _ | D_module _ | D_value _ | D_irrefutable_match _ -> acc
  in
  let module_attr = Ast_core.TypeOrModuleAttr.{ public = true; hidden = true } in
  let attr =
    Ast_core.ValueAttr.
      { inline = true
      ; no_mutation = true
      ; view = false
      ; public = true
      ; hidden = true
      ; thunk = false
      }
  in
  let mod_bindings = List.(rev (fold prg ~init:[] ~f:get_mod_bindings)) in
  let val_bindings = List.(rev (fold prg ~init:[] ~f:get_val_bindings)) in
  let ty_bindings = List.(rev (fold prg ~init:[] ~f:get_ty_bindings)) in
  let prelude_mod : Ast_core.program =
    let open Ast_core in
    let prelude =
      List.map mod_bindings ~f:(fun module_binder ->
          let module_ =
            Location.wrap ~loc @@ Module_expr.M_module_path (mod_binder, [ module_binder ])
          in
          Location.wrap ~loc @@ D_module { module_binder; module_attr; module_ })
    in
    prelude
  in
  let prelude_val : Ast_core.program =
    let open Ast_core in
    let prelude =
      List.map val_bindings ~f:(fun binder ->
          let expr =
            make_e ~loc
            @@ E_module_accessor
                 { module_path = [ mod_binder ]; element = Binder.get_var binder }
          in
          Location.wrap ~loc
          @@ D_value { binder = Binder.set_ascr binder None; attr; expr })
    in
    prelude
  in
  let prelude_ty : Ast_core.program =
    let open Ast_core in
    let prelude =
      List.map ty_bindings ~f:(fun type_binder ->
          let type_expr =
            make_t ~loc
            @@ T_module_accessor { module_path = [ mod_binder ]; element = type_binder }
          in
          Location.wrap ~loc @@ D_type { type_binder; type_expr; type_attr = module_attr })
    in
    prelude
  in
  prelude_mod @ prelude_val @ prelude_ty


(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise
    : Syntax_types.t -> Ast_core.program -> Ast_core.program
  =
 fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise syntax arg in
    let attr =
      ValueAttr.
        { inline = false
        ; no_mutation = true
        ; view = false
        ; public = false
        ; hidden = false
        ; thunk = false
        }
    in
    let d =
      Location.wrap ~loc
      @@ D_value
           { binder = Binder.make (Value_var.of_input_var ~loc "cli_arg") None
           ; expr
           ; attr
           }
    in
    d :: prg
  in
  Option.value_map
    Compiler_options.(options.test_framework.cli_expr_inj)
    ~default:prg
    ~f:inject_arg_declaration
