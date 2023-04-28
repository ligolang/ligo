open Ligo_prim
open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap = Map.Make (Location_ordered)
open Env

type t = string list LMap.t

(** [mvar_to_id] takes and [Module_var.t] and gives id of the form
    [{name}#{line}:{start_col}-{end_col}] *)
let mvar_to_id m =
  let name = Format.asprintf "%a" Module_var.pp m in
  let loc = Module_var.get_location m in
  Types.make_def_id name loc


(** [resolve_mpath] takes a module path [mvs] and tries to resolve in the [env]
    the final output is of type [(Module_var.t * string list) option] 
    it is optional because we are not sure if the module alias can be resolved
    in the [env], in case we are able to resolve the alias the first part 
    [Module_var.t] is the finally resolved module & the [string list] is the list
    of module ids as explained in the comments of the [resolve_module_alias]
    
    This function uses [Env.fold_resolve_mpath] to resolve the module path,
    i.e. for [module X = A.B.C.D] it will initially completely resolve module [A]
    if it is an alias-of-an-alias it will resolve it to the final module definition
    and then look for the rest of the module path [B.C.D] in [env] of the resolved
    module *)
let resolve_mpath : Module_var.t List.Ne.t -> env -> (Module_var.t * string list) option =
 fun mvs env ->
  let init = [] in
  let f : string list -> _ -> string list =
   fun acc (_input, real, _resolved, _defs_of_that_module) -> mvar_to_id real :: acc
  in
  let defs = env.avail_defs @ env.parent in
  let mmap = env.module_map in
  let acc, m_opt = Env.fold_resolve_mpath mvs defs mmap ~init ~f in
  match m_opt with
  | None -> None
  | Some (_real, resolved, _defs) -> Some (resolved, List.rev acc)


(** [resolve_module_alias] completely resolves the module_path [mv] to a module def
    using the [env] and add an entry in [m_alias] with key as lhs module name & the
    value is a list of module ids. 
    For example.
    {[
        module A = struct 
          module B = struct 
            let x = 1
          end
          module C = B
        end
        module D = A

        module E = D.C
    ]}
    In case of [module E = D.C], [D.C] will be resolved to module definitions ids
    [D#8-8:9; C#5-10-11]
    *)
let resolve_module_alias
    : Module_var.t -> Module_var.t List.Ne.t -> env -> t -> t * defs_or_alias option
  =
 fun lhs_mv mvs env m_alias ->
  let ma_res = resolve_mpath mvs env in
  match ma_res with
  | Some (ma, resolved_ids) ->
    let m_alias = LMap.add (Module_var.get_location lhs_mv) resolved_ids m_alias in
    m_alias, Some (Alias ma)
  | None -> m_alias, None


(** [expression] walks the expression and builds the [env] and looks for local 
    module [E_mod_in] calls [module_expression] to resolve that module.  *)
let rec expression : AST.expression -> t -> env -> t =
 fun e m_alias env ->
  match e.expression_content with
  | E_variable _ -> m_alias
  | E_module_accessor _ -> m_alias
  | E_literal _ -> m_alias
  | E_constant { arguments; cons_name = _ } ->
    List.fold arguments ~init:m_alias ~f:(fun m_alias arg -> expression arg m_alias env)
  | E_application { lamb; args } ->
    let m_alias = expression lamb m_alias env in
    expression args m_alias env
  | E_lambda { binder = _; output_type = _; result } -> expression result m_alias env
  | E_recursive
      { fun_name = _
      ; fun_type = _
      ; lambda = { binder = _; output_type = _; result }
      ; force_lambdarec = _
      } -> expression result m_alias env
  | E_type_abstraction { type_binder = _; result } -> expression result m_alias env
  | E_let_mut_in { let_binder = _; rhs; let_result; attributes = _ }
  | E_let_in { let_binder = _; rhs; let_result; attributes = _ } ->
    let m_alias = expression rhs m_alias env in
    expression let_result m_alias env
  | E_type_in { type_binder = _; rhs = _; let_result } ->
    expression let_result m_alias env
  | E_raw_code { language = _; code } -> expression code m_alias env
  | E_constructor { constructor = _; element } -> expression element m_alias env
  | E_matching { matchee; cases } ->
    let m_alias = expression matchee m_alias env in
    List.fold cases ~init:m_alias ~f:(fun m_alias { pattern = _; body } ->
        expression body m_alias env)
  | E_record e_label_map ->
    let es = Record.values e_label_map in
    List.fold es ~init:m_alias ~f:(fun m_alias e -> expression e m_alias env)
  | E_accessor { struct_; path = _ } -> expression struct_ m_alias env
  | E_update { struct_; path = _; update } ->
    let m_alias = expression struct_ m_alias env in
    expression update m_alias env
  | E_ascription { anno_expr; type_annotation = _ } -> expression anno_expr m_alias env
  | E_assign { binder = _; expression = e } -> expression e m_alias env
  | E_for { binder = _; start; final; incr; f_body } ->
    let m_alias = expression start m_alias env in
    let m_alias = expression final m_alias env in
    let m_alias = expression incr m_alias env in
    expression f_body m_alias env
  | E_for_each { fe_binder = _v, _v_opt; collection; collection_type = _; fe_body } ->
    let m_alias = expression collection m_alias env in
    expression fe_body m_alias env
  | E_while { cond; body } ->
    let m_alias = expression cond m_alias env in
    expression body m_alias env
  | E_mod_in { module_binder; rhs; let_result } ->
    let m_alias, defs_or_alias_opt, module_map =
      module_expression module_binder rhs m_alias env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    expression let_result m_alias env


(** [module_expression] walks the module expression and builds the [env] and
    looks for module aliase and calls [resolve_module_alias] on it. *)
and module_expression
    : Module_var.t -> AST.module_expr -> t -> env -> t * defs_or_alias option * module_map
  =
 fun lhs_mv me m_alias env ->
  let env = { env with avail_defs = []; parent = env.avail_defs @ env.parent } in
  let m_alias, defs_or_alias_opt, env =
    match me.wrap_content with
    | M_struct decls ->
      let m_alias, env = declarations decls m_alias env in
      m_alias, Some (Defs env.avail_defs), env
    | M_variable mv ->
      let m_alias, alias_opt = resolve_module_alias lhs_mv (mv, []) env m_alias in
      m_alias, alias_opt, env
    | M_module_path mvs ->
      let m_alias, alias_opt = resolve_module_alias lhs_mv mvs env m_alias in
      m_alias, alias_opt, env
  in
  m_alias, defs_or_alias_opt, env.module_map


(** [declaration] builds the [env] and tries to resolves module aliases *)
and declaration : AST.declaration -> t -> env -> t * env =
 fun d m_alias env ->
  match d.wrap_content with
  | D_value { binder = _; expr; attr = _ } ->
    let m_alias = expression expr m_alias env in
    m_alias, env
  | D_irrefutable_match { pattern = _; expr; attr = _ } ->
    let m_alias = expression expr m_alias env in
    m_alias, env
  | D_type { type_binder = _; type_expr = _; type_attr = _ } -> m_alias, env
  | D_module { module_binder; module_; module_attr = _ } ->
    let m_alias, defs_or_alias_opt, module_map =
      module_expression module_binder module_ m_alias env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    m_alias, env


(** [declarations] builds the [env] and tries to resolves module aliases *)
and declarations : AST.declaration list -> t -> env -> t * env =
 fun decls m_alias env ->
  let m_alias, env =
    List.fold decls ~init:(m_alias, env) ~f:(fun (m_alias, env) decl ->
        declaration decl m_alias env)
  in
  m_alias, env


(** [declarations] sets up the initial env and calls [declarations] *)
let declarations : AST.declaration list -> t =
 fun decls ->
  let m_alias = LMap.empty in
  let env = Env.empty in
  let m_alias, _ = declarations decls m_alias env in
  m_alias


(** [patch] fixes the module aliases in the [defs], It looks for module aliase
    definitions & then looks up the range of the module definition in [t] *)
let rec patch : t -> Types.def list -> Types.def list =
 fun m_alias defs ->
  let open Types in
  List.map defs ~f:(fun def ->
      match def with
      | Module m ->
        let mod_case =
          match m.mod_case with
          | Alias a ->
            (match LMap.find_opt m.range m_alias with
            | Some a -> Types.Alias a
            | None -> Types.Alias a)
          | Def defs -> Def (patch m_alias defs)
        in
        Module { m with mod_case }
      | _ as others -> others)
