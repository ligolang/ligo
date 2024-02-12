open Simple_utils
open Ligo_prim
open Types
module LMap = Types.LMap
module Pattern = Ast_typed.Pattern

type t =
  { type_cases : type_case LMap.t
  ; label_cases : Ast_core.ty_expr LMap.t
  ; module_signatures : signature_case LMap.t
  ; module_env : Env.t
  }

(** For debugging. *)
let pp : t Fmt.t =
 fun ppf { type_cases; module_signatures; module_env; label_cases } ->
  Format.fprintf
    ppf
    "{ type_cases: %a\n; label_cases: %a\n; module_signatures: %a\n; module_env: %a\n}"
    Fmt.Dump.(seq (pair Location.pp PP.type_case))
    (LMap.to_seq type_cases)
    Fmt.Dump.(seq (pair Location.pp Ast_core.PP.type_expression))
    (LMap.to_seq label_cases)
    Fmt.Dump.(seq (pair Location.pp PP.signature_case))
    (LMap.to_seq module_signatures)
    Env.pp
    module_env


let empty (module_env : Env.t) =
  { type_cases = LMap.empty
  ; label_cases = LMap.empty
  ; module_signatures = LMap.empty
  ; module_env
  }


let add_type_case loc type_case env =
  { env with type_cases = LMap.add loc type_case env.type_cases }


let add_label_case loc label_case env =
  { env with
    label_cases =
      LMap.update
        loc
        (function
          | Some t -> Some t
          | None -> Some label_case)
        env.label_cases
  }


let add_module_signature loc signature env =
  { env with
    module_signatures =
      LMap.update
        loc
        (fun old ->
          Option.some
          @@ Option.value_map
               ~default:signature
               ~f:(fun old ->
                 (* Signatures in CameLIGO and JsLIGO have different meanings.
                  In CameLIGO they're used to declare what would be exported from the module.

                  In JsLIGO they declare a set of things that should be present in the namespace,
                  so, the resulting signature could be wider than in `implements` section.

                  Typed signatures already handle this, so, we'll do the next hack:
                  1. If we want to add a resolved signature then put it in the map.
                  2. Otherwise add it only when the key is absent.*)
                 match signature with
                 | Resolved _ -> signature
                 | Core _ | Unresolved -> old)
               old)
        env.module_signatures
  }


(** Collects labels with types that they belong from patterns. *)
let label_bindings : _ Pattern.t -> Ast_core.ty_expr -> (Label.t * Ast_core.ty_expr) list =
 fun p ty_expr ->
  let rec aux
      :  _ Pattern.t -> Ast_core.ty_expr -> (Label.t * Ast_core.ty_expr) list
      -> (Label.t * Ast_core.ty_expr) list
    =
   fun p ty_expr acc ->
    let process
        :  Label.t * _ Pattern.t -> Ast_core.ty_expr Row.With_optional_layout.t
        -> (Label.t * Ast_core.ty_expr) list -> (Label.t * Ast_core.ty_expr) list
      =
     fun (label, inner) row acc ->
      let open Option.Let_syntax in
      Option.value
        ~default:acc
        (let%bind field_type = Row.With_optional_layout.find_type row label in
         return ((label, ty_expr) :: aux inner field_type acc))
    in
    let ty_expr = Ast_core.strip_abstraction ty_expr in
    let get_t_list : Ast_core.ty_expr -> Ast_core.ty_expr option =
     fun ty_expr ->
      match ty_expr.type_content with
      | T_app { type_operator = { module_path = []; element }; arguments = [ argument ] }
        when not (Type_var.is_generated element) ->
        Option.some_if (String.equal (Type_var.to_name_exn element) "list") argument
      | _ -> None
    in
    match p.wrap_content, ty_expr.type_content with
    | P_variant (label, inner), T_sum (t_sum, _) -> process (label, inner) t_sum acc
    | P_record p_record, T_record t_record ->
      List.fold_left
        ~init:acc
        ~f:(fun acc label_and_inner -> process label_and_inner t_record acc)
        (Record.to_list p_record)
    | P_tuple p_tuple, T_record t_record when Row.With_optional_layout.is_tuple t_record
      ->
      let t_tuple = Row.With_optional_layout.to_tuple t_record in
      let elt_and_type, _ = Core.List.zip_with_remainder p_tuple t_tuple in
      List.fold_left ~init:acc ~f:(fun acc (elt, typ) -> aux elt typ acc) elt_and_type
    | P_list (Cons (x, xs)), _ ->
      Option.value
        ~default:acc
        (let open Option.Let_syntax in
        let%bind inner = get_t_list ty_expr in
        return @@ aux x inner @@ aux xs ty_expr acc)
    | P_list (List lst), _ ->
      Option.value
        ~default:acc
        (let open Option.Let_syntax in
        let%bind inner = get_t_list ty_expr in
        return @@ List.fold_left ~init:acc ~f:(fun acc elt -> aux elt inner acc) lst)
    | _, _ -> acc
  in
  aux p ty_expr []


let lookup_signature : Location.t -> t -> signature_case option =
 fun key { module_signatures; _ } -> LMap.find_opt key module_signatures


let resolve_module_path : Module_var.t Types.List.Ne.t -> t -> Module_var.t option =
 fun path { module_env; _ } ->
  let defs = module_env.avail_defs @ module_env.parent in
  let mmap = module_env.module_map in
  Option.map ~f:(fun (_, resolved, _) -> resolved) (Env.resolve_mpath path defs mmap)


module Of_Ast_typed = struct
  type binding =
    | Type_binding of (Ast_typed.expression_variable * Ast_typed.type_expression)
    | Label_binding of (Label.t * Ast_core.type_expression)
    | Module_binding of (Ast_typed.module_variable * Ast_typed.signature)

  let label_bindings : _ Pattern.t -> Ast_typed.ty_expr -> binding list =
   fun p ty_expr ->
    List.map ~f:(fun elt -> Label_binding elt)
    @@ label_bindings p (Checking.untype_type_expression ~use_orig_var:false ty_expr)


  let mk_label_binding : Label.t -> Ast_typed.ty_expr -> binding =
   fun label ty_expr ->
    Label_binding (label, Checking.untype_type_expression ~use_orig_var:false ty_expr)


  let add_binding : t -> binding -> t =
   fun env -> function
    | Type_binding (v, t) ->
      let t =
        match t.orig_var with
        | Some (path, t') -> { t with type_content = T_variable t' }
        | None -> t
      in
      let loc = Value_var.get_location v in
      let type_case = Resolved t in
      add_type_case loc type_case env
    | Label_binding (Label (_, loc), t) -> add_label_case loc t env
    | Module_binding (v, s) ->
      let loc = Module_var.get_location v in
      add_module_signature loc (Resolved s) env


  let add_bindings env bindings = List.fold bindings ~init:env ~f:add_binding

  let rec extract_binding_types_from_signature : t -> Ast_typed.signature -> t =
   fun bindings sig_ ->
    List.fold sig_.sig_items ~init:bindings ~f:(fun bindings sig_item ->
        match Location.unwrap sig_item with
        | S_value (v, t, _) -> add_bindings bindings [ Type_binding (v, t) ]
        | S_type _ -> bindings
        | S_type_var _ -> bindings
        | S_module (v, sig_) ->
          let bindings = add_bindings bindings [ Module_binding (v, sig_) ] in
          extract_binding_types_from_signature bindings sig_
        | S_module_type (v, sig_) -> add_bindings bindings [ Module_binding (v, sig_) ])


  let rec extract_module_content : t -> Ast_typed.module_content -> t =
   fun prev -> function
    | M_variable _ -> prev
    | M_module_path _ -> prev
    | M_struct ds ->
      List.fold_left ds ~init:prev ~f:(fun prev d ->
          extract_binding_types prev d.wrap_content)


  and extract_binding_types : t -> Ast_typed.declaration_content -> t =
   fun prev ->
    let aux : t -> Ast_typed.expression -> t =
     fun env exp ->
      let return = add_bindings env in
      let loc = exp.location in
      match exp.expression_content with
      | E_literal _
      | E_application _
      | E_raw_code _
      | E_assign _
      | E_deref _
      | E_while _
      | E_type_abstraction _
      | E_contract _
      | E_constant _ -> return []
      | E_type_inst _ -> return []
      | E_coerce _ -> return []
      | E_variable v -> return [ Type_binding (v, exp.type_expression) ]
      | E_record record ->
        let labels = Record.labels record in
        return
        @@ List.map ~f:(fun label -> mk_label_binding label exp.type_expression) labels
      | E_accessor { struct_; path } | E_update { struct_; path; update = _ } ->
        return [ mk_label_binding path struct_.type_expression ]
      | E_constructor { constructor; element = _ } ->
        return [ mk_label_binding constructor exp.type_expression ]
      | E_lambda { binder; _ } ->
        return [ Type_binding (Param.get_var binder, Param.get_ascr binder) ]
      | E_recursive { fun_name; fun_type; lambda = { binder; _ }; force_lambdarec = _ } ->
        return
          [ Type_binding (fun_name, fun_type)
          ; Type_binding (Param.get_var binder, Param.get_ascr binder)
          ]
      | E_let_mut_in { let_binder; rhs; _ } | E_let_in { let_binder; rhs; _ } ->
        let labels = label_bindings let_binder rhs.type_expression in
        return
        @@ List.map
             ~f:(fun binder ->
               Type_binding (Binder.get_var binder, Binder.get_ascr binder))
             (Pattern.binders let_binder)
        @ labels
      | E_matching { matchee; disc_label = _; cases } ->
        let ty_expr = matchee.type_expression in
        let pats = List.map ~f:(fun elt -> elt.pattern) cases in
        let labels = List.concat_map ~f:(fun elt -> label_bindings elt ty_expr) pats in
        let bindings =
          List.concat_map cases ~f:(fun { pattern; _ } ->
              let binders = Pattern.binders pattern in
              List.map binders ~f:(fun b ->
                  Type_binding (Binder.get_var b, Binder.get_ascr b)))
        in
        return (bindings @ labels)
      | E_module_accessor { element = e; _ } ->
        return [ Type_binding (e, exp.type_expression) ]
      | E_for { binder; start; _ } ->
        return [ Type_binding (binder, start.type_expression) ]
      | E_for_each { fe_binder = binder1, Some binder2; collection; _ } ->
        let key_type, val_type = Ast_typed.get_t_map_exn collection.type_expression in
        return [ Type_binding (binder1, key_type); Type_binding (binder2, val_type) ]
      | E_for_each { fe_binder = binder, None; collection; _ } ->
        let type_ = collection.type_expression in
        if Ast_typed.is_t_set type_
        then return [ Type_binding (binder, Ast_typed.get_t_set_exn type_) ]
        else if Ast_typed.is_t_list type_
        then return [ Type_binding (binder, Ast_typed.get_t_list_exn type_) ]
        else if Ast_typed.is_t_map type_
        then (
          let k, v = Ast_typed.get_t_map_exn type_ in
          return [ Type_binding (binder, Ast_typed.t_pair ~loc k v) ])
        else return []
      | E_mod_in { rhs = { signature; _ }; module_binder; _ } ->
        let env = return [ Module_binding (module_binder, signature) ] in
        extract_binding_types_from_signature env signature
    in
    function
    | D_value { attr = { hidden = true; _ }; _ } -> prev
    | D_irrefutable_match { attr = { hidden = true; _ }; _ } -> prev
    | D_value { binder; expr; _ } ->
      let prev =
        add_bindings prev [ Type_binding (Binder.get_var binder, Binder.get_ascr binder) ]
      in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | D_irrefutable_match { pattern; expr; _ } ->
      let prev = add_bindings prev (label_bindings pattern expr.type_expression) in
      let prev =
        let f acc binder =
          add_bindings
            acc
            [ Type_binding (Binder.get_var binder, Binder.get_ascr binder) ]
        in
        List.fold (Pattern.binders pattern) ~f ~init:prev
      in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | D_type _ -> prev
    | D_module { module_binder; module_; annotation = (); module_attr = _ } ->
      let prev =
        add_bindings prev [ Module_binding (module_binder, module_.signature) ]
      in
      extract_module_content prev module_.module_content
    | D_module_include { module_content; module_location = _; signature = _ } ->
      extract_module_content prev module_content
    | D_signature { signature_binder; signature; signature_attr = _ } ->
      let prev = add_bindings prev [ Module_binding (signature_binder, signature) ] in
      extract_binding_types_from_signature prev signature
end

module Of_Ast_core = struct
  let add_binding_in_map : t -> Location.t * type_case -> t =
   fun env (loc, type_case) -> add_type_case loc type_case env


  let add_label_in_map : t -> Location.t * Ast_core.ty_expr -> t =
   fun env (loc, type_case) -> add_label_case loc type_case env


  let add_bindings_in_map : t -> (Location.t * type_case) list -> t =
   fun env -> List.fold ~init:env ~f:add_binding_in_map


  (* S_include is kinda weird thing. After Ast_unified -> Ast_core they occur in JsLIGO
    and it's impossible to decompile them back into CST. So, it would be convenient to inline them. *)
  let rec inline_generated_include_sig_item
      : t -> Ast_typed.signature -> Ast_core.sig_item -> Ast_core.sig_item list
    =
   fun env prg_sig sig_item ->
    let loc = Location.get_location sig_item in
    match Location.unwrap sig_item with
    | S_value _ | S_type _ | S_type_var _ -> [ sig_item ]
    | S_module (mvar, sig_) ->
      [ Location.wrap ~loc
        @@ Ast_core.S_module (mvar, inline_generated_include_signature env prg_sig sig_)
      ]
    | S_module_type (mvar, sig_) ->
      [ Location.wrap ~loc
        @@ Ast_core.S_module_type
             (mvar, inline_generated_include_signature env prg_sig sig_)
      ]
    | S_include { wrap_content = sig_expr; location = _ } ->
      let f = inline_generated_include_sig_item env prg_sig in
      (match sig_expr with
      | S_sig { items } -> List.concat_map ~f items
      | S_path path ->
        Option.value
          ~default:[]
          (let open Simple_utils.Option in
          let* resolved_module = resolve_module_path path env in
          let* signature_case =
            lookup_signature (Module_var.get_location resolved_module) env
          in
          match signature_case with
          | Resolved sig_ ->
            return @@ (Checking.untype_signature ~use_orig_var:true sig_).items
          | Core { items } -> return @@ List.concat_map ~f items
          | Unresolved -> None))


  and inline_generated_include_signature
      : t -> Ast_typed.signature -> Ast_core.signature -> Ast_core.signature
    =
   fun env prg_sig { items } ->
    { items = List.concat_map items ~f:(inline_generated_include_sig_item env prg_sig) }


  let add_module_signature
      : t -> Ast_typed.signature -> Module_var.t -> Ast_core.signature_expr option -> t
    =
   fun env prg_sig v -> function
    | Some { wrap_content = S_sig signature; _ } ->
      add_module_signature
        (Module_var.get_location v)
        (Core (inline_generated_include_signature env prg_sig signature))
        env
    | Some { wrap_content = S_path path; _ } ->
      Option.value
        ~default:env
        (let open Simple_utils.Option in
        let* resolved_module = resolve_module_path path env in
        let* signature_case =
          lookup_signature (Module_var.get_location resolved_module) env
        in
        return @@ add_module_signature (Module_var.get_location v) signature_case env)
    | None -> env


  let add_binders : t -> Ast_core.type_expression option Binder.t list -> t =
   fun env bindings ->
    let bindings =
      List.filter_map bindings ~f:(fun binder ->
          let%map.Option t = Binder.get_ascr binder in
          Binder.get_loc binder, Core t)
    in
    add_bindings_in_map env bindings


  let add_binder : t -> Ast_core.type_expression Binder.t -> t =
   fun env binding ->
    let loc = Binder.get_loc binding in
    let t = Core (Binder.get_ascr binding) in
    add_binding_in_map env (loc, t)


  let add_vvar_type : t -> Value_var.t * Ast_core.type_expression -> t =
   fun env (v, t) ->
    let loc = Value_var.get_location v in
    let t = Core t in
    add_binding_in_map env (loc, t)


  let add_label_type : t -> Label.t * Ast_core.type_expression -> t =
   fun env (Label (_, loc), t) -> add_label_in_map env (loc, t)


  let add_param_type : t -> Ast_core.type_expression Param.t -> t =
   fun env param -> add_vvar_type env (Param.get_var param, Param.get_ascr param)


  let add_param_type_opt : t -> Ast_core.type_expression option Param.t -> t =
   fun env param ->
    match Param.get_ascr param with
    | None -> env
    | Some t -> add_vvar_type env (Param.get_var param, t)


  (** [set_core_type_if_possible] detects patterns like

      {[
        let x : int = 1
      ]}

      The abstraction pass will create AST like
      [D_irrefutable_match
        (E_ascription ({ expr = E_literal 1; type_annotation = int }), _)]

      So here we want the x to have [Core int]

      Hence we extract the type annotation from the rhs and we set it back to
      the binder.
  *)
  let set_core_type_if_possible
      :  dyn_entry:bool -> Ast_core.type_expression option Binder.t list
      -> Ast_core.expression
      -> Ast_core.type_expression option Binder.t list * Ast_core.expression
    =
   fun ~dyn_entry binders expr ->
    let binders =
      (* We don't want to get core type for dynamic entrypoints
        because it'll result in the wrong hover
        (i.e. we'll get `'p -> 's -> operation list * 's`
        instead of `('p, 's) dynamic_entrypoint`). *)
      if dyn_entry
      then List.map binders ~f:(fun binder -> Binder.set_ascr binder None)
      else binders
    in
    match binders, expr.expression_content with
    | [ binder ], Ast_core.E_ascription { anno_expr = _; type_annotation }
      when not dyn_entry ->
      let binder = Binder.set_ascr binder (Some type_annotation) in
      [ binder ], expr
    | _ -> binders, expr


  let rec expression : Ast_typed.signature -> t -> Ast_core.expression -> t =
   fun prg_sig bindings expr ->
    let expression = expression prg_sig in
    let get_type : Ast_core.expression -> Ast_core.ty_expr option =
     fun expr ->
      match expr.expression_content with
      | E_ascription { anno_expr = _; type_annotation } -> Some type_annotation
      | _ -> None
    in
    match expr.expression_content with
    | E_literal _ | E_variable _ | E_module_accessor _ | E_contract _ -> bindings
    | E_raw_code { code; _ } -> expression bindings code
    | E_constant { arguments; _ } -> List.fold arguments ~init:bindings ~f:expression
    | E_application { lamb; args } ->
      let bindings = expression bindings lamb in
      expression bindings args
    | E_type_abstraction { result; _ } -> expression bindings result
    | E_type_in { let_result; _ } -> expression bindings let_result
    | E_constructor { element; _ } -> expression bindings element
    | E_record lmap -> Record.fold lmap ~init:bindings ~f:expression
    | E_accessor { struct_; path } ->
      let bindings =
        Option.value_map
          ~default:bindings
          ~f:(fun ty_expr -> add_label_type bindings (path, ty_expr))
          (get_type struct_)
      in
      expression bindings struct_
    | E_update { struct_; update; path } ->
      let bindings =
        Option.value_map
          ~default:bindings
          ~f:(fun ty_expr -> add_label_type bindings (path, ty_expr))
          (get_type struct_)
      in
      let bindings = expression bindings struct_ in
      expression bindings update
    | E_ascription { anno_expr; type_annotation } ->
      let bindings =
        match anno_expr.expression_content with
        | E_record record ->
          let labels = Record.labels record in
          List.fold_left
            ~init:bindings
            ~f:(fun acc label -> add_label_type acc (label, type_annotation))
            labels
        | E_constructor { constructor; element = _ } ->
          add_label_type bindings (constructor, type_annotation)
        | _ -> bindings
      in
      expression bindings anno_expr
    | E_assign { binder; expression = e } ->
      let bindings = add_binders bindings [ binder ] in
      expression bindings e
    | E_for { start; final; incr; f_body; _ } ->
      let bindings = expression bindings start in
      let bindings = expression bindings final in
      let bindings = expression bindings incr in
      expression bindings f_body
    | E_for_each { collection; fe_body; _ } ->
      let bindings = expression bindings collection in
      expression bindings fe_body
    | E_while { cond; body } ->
      let bindings = expression bindings cond in
      expression bindings body
    | E_lambda { binder; result; _ } ->
      let bindings = add_param_type_opt bindings binder in
      expression bindings result
    | E_recursive { fun_name; fun_type; lambda = { binder; result; _ }; _ } ->
      let bindings = add_vvar_type bindings (fun_name, fun_type) in
      let bindings = add_param_type bindings binder in
      expression bindings result
    | E_matching { matchee; disc_label = _; cases } ->
      let bindings =
        Option.value_map
          ~default:bindings
          ~f:(fun ty_expr ->
            let pats = List.map ~f:(fun elt -> elt.pattern) cases in
            let labels =
              List.concat_map ~f:(fun pat -> label_bindings pat ty_expr) pats
            in
            List.fold_left ~init:bindings ~f:add_label_type labels)
          (get_type matchee)
      in
      let bindings = expression bindings matchee in
      List.fold cases ~init:bindings ~f:(fun bindings { pattern; body } ->
          let bindings = expression bindings body in
          let binders = Pattern.binders pattern in
          add_binders bindings binders)
    | E_let_mut_in { let_binder; rhs; let_result; attributes = { dyn_entry; _ } }
    | E_let_in { let_binder; rhs; let_result; attributes = { dyn_entry; _ } } ->
      let bindings =
        Option.value_map
          ~default:bindings
          ~f:(fun ty_expr ->
            let labels = label_bindings let_binder ty_expr in
            List.fold_left ~init:bindings ~f:add_label_type labels)
          (get_type rhs)
      in
      let binders = Pattern.binders let_binder in
      let binders, rhs = set_core_type_if_possible ~dyn_entry binders rhs in
      let bindings = add_binders bindings binders in
      let bindings = expression bindings rhs in
      expression bindings let_result
    | E_mod_in { rhs; let_result; _ } ->
      let bindings = module_expr_content prg_sig bindings (Location.unwrap rhs) in
      expression bindings let_result


  and signature_expr : t -> Ast_core.signature_expr -> t =
   fun bindings sig_expr -> signature_content bindings (Location.unwrap sig_expr)


  and sig_item : t -> Ast_core.sig_item -> t =
   fun bindings sig_item ->
    match Location.unwrap sig_item with
    | S_value (var, ty_expr, _attrs) ->
      let binder = Binder.make var ty_expr in
      add_binder bindings binder
    | S_type (_var, _ty_expr, _) -> bindings
    | S_type_var (_var, _) -> bindings
    | S_module (_var, _sig) -> bindings
    | S_module_type (_var, _sig) -> bindings
    | S_include signature -> signature_expr bindings signature


  and signature_content : t -> Ast_core.signature_content -> t =
   fun bindings -> function
    | S_sig items -> sig_items bindings items.items
    | S_path _ -> bindings


  and module_expr_content : Ast_typed.signature -> t -> Ast_core.module_expr_content -> t =
   fun prg_sig bindings -> function
    | M_struct decls -> declarations bindings prg_sig decls
    | M_variable _ | M_module_path _ -> bindings


  and declaration : Ast_typed.signature -> t -> Ast_core.declaration -> t =
   fun prg_sig bindings decl ->
    match Location.unwrap decl with
    | D_value { binder; expr; attr = { dyn_entry; _ } } ->
      let binders, expr = set_core_type_if_possible ~dyn_entry [ binder ] expr in
      let bindings = add_binders bindings binders in
      expression prg_sig bindings expr
    | D_irrefutable_match { pattern; expr; attr = { dyn_entry; _ } } ->
      let bindings =
        match expr.expression_content with
        | E_ascription { anno_expr = _; type_annotation } ->
          let labels = label_bindings pattern type_annotation in
          List.fold_left ~init:bindings ~f:add_label_type labels
        | _ -> bindings
      in
      let binders = Pattern.binders pattern in
      let binders, expr = set_core_type_if_possible ~dyn_entry binders expr in
      let bindings = add_binders bindings binders in
      expression prg_sig bindings expr
    | D_type _ -> bindings
    | D_module
        { module_binder
        ; module_ = { wrap_content; location = _ }
        ; annotation
        ; module_attr = _
        } ->
      let bindings =
        add_module_signature
          bindings
          prg_sig
          module_binder
          (Option.map ~f:(fun annotation -> annotation.signature) annotation)
      in
      let bindings = module_expr_content prg_sig bindings wrap_content in
      (match annotation with
      | None -> bindings
      | Some { signature = { wrap_content; location = _ }; filter = _ } ->
        signature_content bindings wrap_content)
    | D_module_include _ -> bindings (* TODO *)
    | D_signature
        { signature_binder
        ; signature = { wrap_content; location = _ } as signature
        ; signature_attr = _
        } ->
      let bindings =
        add_module_signature bindings prg_sig signature_binder (Some signature)
      in
      signature_content bindings wrap_content


  and declarations : t -> Ast_typed.signature -> Ast_core.declaration list -> t =
   fun bindings prg_sig -> List.fold ~init:bindings ~f:(declaration prg_sig)


  and sig_items : t -> Ast_core.sig_item list -> t =
   fun bindings -> List.fold ~init:bindings ~f:sig_item
end

module Typing_env = struct
  let rec add_stdlib_modules : t -> Ast_typed.signature -> t =
   fun env stdlib_sig ->
    List.fold_left stdlib_sig.sig_items ~init:env ~f:(fun env -> function
      | { wrap_content = S_module (name, sig_) | S_module_type (name, sig_)
        ; location = _
        } ->
        let env = add_stdlib_modules env sig_ in
        Of_Ast_typed.add_binding env (Module_binding (name, sig_))
      | _ -> env)


  type nonrec t =
    { type_env : Ast_typed.signature
    ; bindings : t
    ; decls : Ast_typed.declaration list
    }

  (** For debugging. *)
  let[@warning "-32"] pp : t Fmt.t =
   fun ppf { type_env; bindings; decls } ->
    Format.fprintf
      ppf
      "{ type_env: %a\n; bindings: %a\n; decls: %a\n}"
      Ast_typed.PP.signature
      type_env
      pp
      bindings
      Fmt.Dump.(list Ast_typed.PP.declaration)
      decls


  (** The typer normall call {!Trace.error} which internally always calls {!Stdlib.raise}
      which stops the program, But here we want to recover from the error. That is the reason
      we use {!collect_warns_and_errs} *)
  let collect_warns_and_errs
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      tracer
      (e, ws)
    =
    let () = List.iter ws ~f:raise.warning in
    raise.log_error (tracer e)


  let update_typing_env
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      ~options
      tenv
      decl
    =
    let typed_prg =
      Simple_utils.Trace.to_stdlib_result
      @@ Checking.type_declaration ~options ~env:tenv.type_env ~path:[] decl
    in
    match typed_prg with
    | Ok (decls, ws) ->
      List.fold_left decls ~init:tenv ~f:(fun tenv decl ->
          let bindings =
            Of_Ast_typed.extract_binding_types tenv.bindings decl.wrap_content
          in
          let type_env =
            { tenv.type_env with
              sig_items = tenv.type_env.sig_items @ Ast_typed.Misc.to_sig_items [ decl ]
            }
          in
          let decls = tenv.decls @ [ decl ] in
          let () = List.iter ws ~f:raise.warning in
          { type_env; bindings; decls })
    | Error (e, ws) ->
      collect_warns_and_errs ~raise Main_errors.checking_tracer (e, ws);
      tenv


  (* If our file contains entrypoints, we should type-check them (e.g. check that
     they have the same storage), and attach corresponding storage and parameter
     to our program, so they will be available during self_ast_typed_pass *)
  let signature_sort_pass
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      ~(options : Compiler_options.middle_end)
      ~(loc : Location.t)
      (tenv : t)
      : t
    =
    let sig_sort : Ast_typed.sig_sort =
      match
        Simple_utils.Trace.to_stdlib_result
        @@ Checking.eval_signature_sort ~options ~loc ~path:[] tenv.type_env
      with
      | Ok (sig_sort, ws) ->
        List.iter ws ~f:raise.warning;
        sig_sort
      | Error (e, ws) ->
        collect_warns_and_errs ~raise Main_errors.checking_tracer (e, ws);
        Ss_module
    in
    { tenv with type_env = { tenv.type_env with sig_sort } }


  let self_ast_typed_pass
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      ~(options : Compiler_options.middle_end)
      tenv
    =
    ignore options;
    match
      Simple_utils.Trace.to_stdlib_result
      @@ Self_ast_typed.all_program
           Ast_typed.{ pr_sig = tenv.type_env; pr_module = tenv.decls }
    with
    | Ok (_, ws) -> List.iter ws ~f:raise.warning
    | Error (e, ws) ->
      collect_warns_and_errs ~raise Main_errors.self_ast_typed_tracer (e, ws)


  let init stdlib_decls env =
    let type_env = Ast_typed.Misc.to_signature stdlib_decls in
    { type_env; bindings = add_stdlib_modules (empty env) type_env; decls = stdlib_decls }
end

(** [resolve] takes your [Ast_core.program] and gives you the typing information
    in the form of [t]

    Here we run the typer related thing first because in the following example
    {[
      let x : int = 1
      let t : int = "2"
      let y = x + ""
    ]}

    In the first pass for each [Ast_core.declaration] we [Typing_env.update_typing_env]
    this gives use

    pass1 (just typer) -> [ x -> resolved (int) ; t -> unresolved ; y -> unresolved ]

    After running the typer we traverse the [Ast_core.program] to fill in the [t]
    with type annotations available in the program

    pass2 -> [ x -> resolved (int) ; t -> core (int) ; y -> unresolved ]

    {i Note:} If we do pass2 before pass1 we will end up with
    [ x -> resolved (int) ; t -> unresolved ; y -> unresolved ]
    but the lsp expects the type t should be [int] (which is annotated by the user)
*)
let resolve
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> options:Compiler_options.middle_end -> stdlib_decls:Ast_typed.program
    -> module_env:Env.t -> Ast_core.program -> Typing_env.t
  =
 fun ~raise ~options ~stdlib_decls ~module_env prg ->
  let loc = Checking.loc_of_program prg in
  let tenv = Typing_env.init stdlib_decls.pr_module module_env in
  let tenv = List.fold prg ~init:tenv ~f:(Typing_env.update_typing_env ~raise ~options) in
  let tenv = Typing_env.signature_sort_pass ~raise ~options ~loc tenv in
  let () = Typing_env.self_ast_typed_pass ~raise ~options tenv in
  let bindings = Of_Ast_core.declarations tenv.bindings tenv.type_env prg in
  { tenv with bindings }


let rec patch : t -> Types.def list -> Types.def list =
 fun bindings defs ->
  List.map defs ~f:(function
      | Variable v ->
        (match v.t, LMap.find_opt v.range bindings.type_cases with
        | Unresolved, Some t -> Types.Variable { v with t }
        | _ -> Variable v)
      | Type t -> Type t
      | Module m ->
        let patch_mod_case = function
          | Alias _ as alias -> alias
          | Def defs -> Def (patch bindings defs)
        in
        let patch_implementation = function
          | Ad_hoc_signature defs -> Ad_hoc_signature (patch bindings defs)
          | Standalone_signature_or_module _ as path -> path
        in
        let m =
          match m.signature, LMap.find_opt m.range bindings.module_signatures with
          | Unresolved, Some signature -> { m with signature }
          | _ -> m
        in
        Module
          { m with
            mod_case = patch_mod_case m.mod_case
          ; implements = List.map ~f:patch_implementation m.implements
          }
      | Label l -> Label l)
