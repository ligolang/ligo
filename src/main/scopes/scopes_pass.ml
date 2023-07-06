(* open Types *)

open Ligo_prim
open Simple_utils
open Env
module LSet = Types.LSet
module LMap = Map.Make (Location_ordered)
module Trace = Simple_utils.Trace
module AST = Ast_core

type t = def list LMap.t

let add : t -> Location.t -> def list -> t =
 fun scopes rhs_range defs ->
  let f : def list option -> def list option = function
    | None -> Some defs
    | Some value -> Some (List.rev_append defs value)
  in
  LMap.update rhs_range f scopes


(* --------------------------- AST traversal -------------------------------- *)

module Of_Ast = struct
  (**
  [env_changed] determines whether a scopes entry should be added for the current expression.

  When an expression contains several subexpression, all with same environment,
  the entry should not be added for those subexpressions.
  Otherwise, we'll have several scopes entries with same content, and whose location include each other.

  However, if the environment of a subexpression differs from its parent
  (e.g. the body of a let-in), then it should be added.

  The [env_changed] flag means that the [env] passed along the recursive call has changed
  since the last call to [add_current_expr].
  Thus, meaning that the next call to [add_current_expr] should not be ignored.

  The general rule for [env_changed] is :
   - At the top-level call, it is set to [true]
   - When [add_current_expr] is called, it is set to [false]
   - When [env] is updated, it is set to [true]
   - Otherwise, it is passed along, unchanged.
   *)
  let rec expression ~(env_changed : bool) : AST.expression -> t -> env -> t =
   fun e scopes env ->
    let self ?(env_changed = env_changed) expr t env =
      expression ~env_changed expr t env
    in
    let add_current_expr scopes =
      if env_changed then add scopes e.location (env.avail_defs @ env.parent) else scopes
    in
    match e.expression_content with
    | E_variable _ -> add_current_expr scopes
    | E_module_accessor _ -> add_current_expr scopes
    (* TODO : Should we recursively call on the maccess.element ? *)
    | E_literal _ -> add_current_expr scopes (* c.f. match.mligo:3 *)
    | E_constant { arguments; cons_name = _ } ->
      (* c.f. constant.mligo,
       NOTE : 4-2 6-14 should be 4-2 6-33 instead *)
      let scopes = add_current_expr scopes in
      (* Then iterate over E_constant's arguments, c.f. constant.mligo *)
      let scopes =
        let init = scopes in
        let f scopes e = self ~env_changed:false e scopes env in
        List.fold arguments ~init ~f
      in
      scopes
    | E_application { lamb; args } ->
      (* TODO : application.mligo has weird scopes, so I'm doing my own interpretation here *)
      (* 1. Add avail defs to the whole expression *)
      let scopes = add_current_expr scopes in
      let scopes = self ~env_changed:false lamb scopes env in
      let scopes = self ~env_changed:false args scopes env in
      scopes
    | E_lambda { binder; output_type = _; result } ->
      (* OK, c.f. lambda.mligo : adding i and j to range of result 'j + i' *)
      let env = Env.add_vvar (Param.get_var binder) env in
      self ~env_changed:true result scopes env
    | E_recursive
        { fun_name
        ; fun_type = _
        ; lambda = { binder; output_type = _; result }
        ; force_lambdarec = _
        } ->
      (* Env logic from References *)
      let env = env |> Env.add_vvar fun_name |> Env.add_vvar (Param.get_var binder) in
      self ~env_changed:true result scopes env
    | E_type_abstraction { type_binder; result } ->
      let env = Env.add_tvar type_binder env in
      self result scopes env
    | E_let_mut_in { let_binder; rhs; let_result; attributes = _ }
    | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
      (* Only recursive call on rhs and body, c.f. letin.mligo *)
      let scopes = self rhs scopes env in
      (* Env logic from References *)
      let env =
        let binders = Linear_pattern.binders let_binder in
        let vars = List.map binders ~f:Binder.get_var in
        List.fold_right vars ~init:env ~f:Env.add_vvar
      in
      let scopes = self ~env_changed:true let_result scopes env in
      scopes
    | E_type_in { type_binder; rhs = _; let_result } ->
      let env = Env.add_tvar type_binder env in
      let scopes = self ~env_changed:true let_result scopes env in
      scopes
    | E_raw_code { language = _; code = _ } -> add_current_expr scopes
    | E_constructor { constructor = _; element } -> self element scopes env
    | E_matching { matchee; cases } ->
      let scopes = self matchee scopes env (* c.f. match.mligo:6 *) in
      (* Env update logic from References *)
      List.fold cases ~init:scopes ~f:(fun scopes { pattern; body } ->
          let binders = Linear_pattern.binders pattern in
          let vars = List.map binders ~f:Binder.get_var in
          let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
          self ~env_changed:true body scopes env (* c.f. match.mligo *))
    | E_record e_label_map ->
      let es = Record.values e_label_map in
      List.fold es ~init:scopes ~f:(fun scopes e -> self e scopes env)
    | E_accessor { struct_; path = _ } -> self struct_ scopes env
    | E_update { struct_; path = _; update } ->
      let scopes = self struct_ scopes env in
      let scopes = self update scopes env in
      scopes
    | E_ascription { anno_expr; type_annotation } ->
      let scopes = type_expression type_annotation scopes env in
      let scopes = self anno_expr scopes env in
      scopes
    | E_assign { binder = _; expression = e } -> self e scopes env
    | E_for { binder; start; final; incr; f_body } ->
      let scopes = self start scopes env in
      let scopes = self final scopes env in
      let scopes = self incr scopes env in
      (* Env update logic from References *)
      let env = Env.add_vvar binder env in
      let scopes = self ~env_changed:true f_body scopes env in
      scopes
    | E_for_each { fe_binder = v, v_opt; collection; collection_type = _; fe_body } ->
      let scopes = self collection scopes env in
      (* Env update logic from References *)
      let env = Env.add_vvar v env in
      let env = Option.fold v_opt ~init:env ~f:(Fn.flip Env.add_vvar) in
      let scopes = self ~env_changed:true fe_body scopes env in
      scopes
    | E_while { cond; body } ->
      let scopes = self cond scopes env in
      let scopes = self body scopes env in
      scopes
    | E_mod_in { module_binder; rhs; let_result } ->
      let scopes, defs_or_alias_opt, module_map = module_expression rhs scopes env in
      (* Env update logic from References *)
      let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
      let scopes = self ~env_changed:true let_result scopes env in
      scopes


  and type_expression : AST.type_expression -> t -> env -> t =
   fun te scopes env ->
    (* TODO : Add scopes entries with avail types, call recursively on T_for_all / T_abstraction *)
    add scopes te.location (env.avail_defs @ env.parent)
  (* match te.type_content with
  | T_variable tv -> 
  | T_module_accessor { module_path; element } ->
  | T_app { type_operator = { module_path; element }; arguments } ->
  | T_singleton _ -> refs
  | T_sum { layout = _; fields } | T_record { layout = _; fields } ->
  | T_arrow { type1; type2 } ->
  | T_for_all { ty_binder = _; kind = _; type_ }
  | T_abstraction { ty_binder = _; kind = _; type_ } -> type_expression type_ refs env *)


  (** [module_expression] takes a [AST.module_expr] and depending on the type of 
    module i.e. alias or struct returns [defs_or_alias] 
     - for a module alias (e.g. module A = B.C.D) it resolves B.C.D to what D points
       to. In case of an alias, it will further resolve until it finds an actual 
       module
     - for a actual module, it returns the list of its [def]'s
    and updates the [references] & the [module_map] *)
  and module_expression
      : AST.module_expr -> t -> env -> t * defs_or_alias option * module_map
    =
   fun me scopes env ->
    (* Env update logic from References *)
    (* Move [avail_defs] to [parent] before finding [references] in module_expr *)
    (* TODO : Move this into a Env.enter_module function or something like that *)
    (* TODO : This update should be done upon call to [declarations], not [module_expression] *)
    let current_defs = env.avail_defs @ env.parent in
    let env = { env with avail_defs = []; parent = current_defs } in
    let refs, defs_or_alias_opt, env =
      match me.wrap_content with
      | M_struct decls ->
        (* Add 1 entry per declaration rhs *)
        let scopes, env = declarations decls scopes env in
        scopes, Some (Defs env.avail_defs), env
      | M_variable mv ->
        let defs_or_alias_opt =
          match Env.resolve_mvar mv current_defs env.module_map with
          | None -> None
          | Some (_, resolved, _) -> Some (Alias resolved)
        in
        scopes, defs_or_alias_opt, env
      | M_module_path mvs ->
        let defs_or_alias_opt =
          match Env.resolve_mpath mvs current_defs env.module_map with
          | None -> None
          | Some (_, resolved, _) -> Some (Alias resolved)
        in
        scopes, defs_or_alias_opt, env
    in
    refs, defs_or_alias_opt, env.module_map


  (** [declaration] takes [AST.declaration] and updates the [references] & [env] *)
  and declaration : AST.declaration -> t -> env -> t * env =
   fun d scopes env ->
    match d.wrap_content with
    | D_value { binder; expr; attr = _ } ->
      (* TODO : Uncomment this, adding scopes entry for type ascription range *)
      (* let scopes =
        Option.fold (Binder.get_ascr binder) ~init:scopes ~f:(fun scopes t ->
            type_expression t scopes env)
      in *)
      let var = Binder.get_var binder in
      if Value_var.is_generated var
      then scopes, env
      else (
        let scopes = expression ~env_changed:true expr scopes env in
        let env = Env.add_vvar var env in
        scopes, env)
    | D_irrefutable_match { pattern; expr; attr = _ } ->
      let binder = Linear_pattern.binders pattern in
      let tys = List.map binder ~f:Binder.get_ascr in
      let scopes =
        List.fold tys ~init:scopes ~f:(fun scopes ty_opt ->
            match ty_opt with
            | None -> scopes
            | Some ty -> type_expression ty scopes env)
      in
      let vars = List.map binder ~f:Binder.get_var in
      let scopes = expression ~env_changed:true expr scopes env in
      let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
      scopes, env
    | D_type { type_binder; type_expr; type_attr = _ } ->
      let scopes = type_expression type_expr scopes env in
      let env = Env.add_tvar type_binder env in
      scopes, env
    | D_module { module_binder; module_; module_attr = _; annotation = _ } ->
      let scopes, defs_or_alias_opt, module_map = module_expression module_ scopes env in
      let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
      scopes, env
    | D_module_include _ | D_signature _ -> scopes, env


  (** [declarations] takes a list of [AST.declaration], [references] & [env]
    and for each delcaration it calls the [delcaration] function with updates
    the [references] & [env] *)
  and declarations : AST.declaration list -> t -> env -> t * env =
   fun decls scopes env ->
    let scopes, env =
      List.fold decls ~init:(scopes, env) ~f:(fun (scopes, env) decl ->
          declaration decl scopes env)
    in
    scopes, env


  let declarations ~(env_preload_decls : AST.declaration list) : AST.declaration list -> t
    =
   fun decls ->
    let scopes = LMap.empty in
    let env = Env.empty in
    let _, env =
      declarations env_preload_decls scopes env (* Preload env with stdlib if provided *)
    in
    let scopes, _ = declarations decls scopes env in
    scopes
end

let to_old_scopes : Types.def list -> t -> Types.scopes =
 fun prg_defs new_scopes ->
  new_scopes |> LMap.map (Env.Def.defs_to_types_defs prg_defs) |> LMap.to_kv_list
