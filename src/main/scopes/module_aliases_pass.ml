open Ligo_prim
open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap = Map.Make (Location_ordered)
open Env

type t = (Types.Uid.t list * Types.Uid.t) LMap.t

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
let resolve_mpath
    : Module_var.t List.Ne.t -> env -> (Module_var.t * Types.Uid.t list) option
  =
 fun mvs env ->
  let init = [] in
  let f : Types.Uid.t list -> _ -> Types.Uid.t list =
   fun acc (_input, real, _resolved, _defs_of_that_module) -> Types.mvar_to_id real :: acc
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

    The [lhs_mv_opt] is optional as it's possible we might be dealing with a signature.
    For example:
    {[
      module type S = sig
        val x : int
      end

      module type A = S

      module M : A = struct
        let x = 1
      end
    ]}

    In the definition for [module M], [A] needs to be resolved to [S#2:13-14]. *)
let resolve_module_alias
    :  Module_var.t option -> Module_var.t List.Ne.t -> env -> t
    -> t * defs_or_alias option
  =
 fun lhs_mv_opt mvs env m_alias ->
  let ma_res = resolve_mpath mvs env in
  match ma_res with
  | Some (ma, resolved_ids) ->
    let resolved_name = Types.mvar_to_id ma in
    let m_alias =
      Option.value_map lhs_mv_opt ~default:m_alias ~f:(fun lhs_mv ->
          LMap.add (Module_var.get_location lhs_mv) (resolved_ids, resolved_name) m_alias)
    in
    m_alias, Some (Alias ma)
  | None -> m_alias, None


(** [expression] walks the expression and builds the [env] and looks for local
    module [E_mod_in] calls [module_expression] to resolve that module. *)
let rec expression : AST.expression -> t -> env -> t =
 fun e m_alias env ->
  match e.expression_content with
  | E_variable _ -> m_alias
  | E_contract _ -> m_alias
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
      module_expression (Some module_binder) rhs m_alias env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    expression let_result m_alias env


(** [module_expression] walks the module expression and builds the [env] and
    looks for module aliases and calls [resolve_module_alias] on it. *)
and module_expression
    :  Module_var.t option -> AST.module_expr -> t -> env
    -> t * defs_or_alias option * module_map
  =
 fun parent_mod me m_alias env ->
  let env =
    { env with avail_defs = []; parent = env.avail_defs @ env.parent; parent_mod }
  in
  let m_alias, defs_or_alias_opt, env =
    match me.wrap_content with
    | M_struct decls ->
      let m_alias, env = declarations decls m_alias env in
      m_alias, Some (Defs env.avail_defs), env
    | M_variable mv ->
      let m_alias, alias_opt = resolve_module_alias parent_mod (mv, []) env m_alias in
      m_alias, alias_opt, env
    | M_module_path mvs ->
      let m_alias, alias_opt = resolve_module_alias parent_mod mvs env m_alias in
      m_alias, alias_opt, env
  in
  m_alias, defs_or_alias_opt, env.module_map


(** [signature_expression] walks the signature expression and builds the [env] and
    looks for signature aliases and calls [resolve_module_alias] on it. *)
and signature_expression : AST.signature_expr -> t -> env -> t * env =
 fun se m_alias env ->
  let m_alias, _defs_or_alias_opt, env =
    match se.wrap_content with
    | S_sig decls ->
      let m_alias, env = signature decls m_alias env in
      m_alias, Some (Defs env.avail_defs), env
    | S_path path ->
      let m_alias, alias_opt = resolve_module_alias None path env m_alias in
      m_alias, alias_opt, env
  in
  m_alias, env


(** [signature_expression_from_D_signature] is like [signature_expression] but with
    similar considerations of [Definitions.mod_cases_of_sig_expr_from_D_signature]. *)
and signature_expression_from_D_signature
    :  Module_var.t option -> AST.signature_expr -> t -> env
    -> t * defs_or_alias option * module_map
  =
 fun parent_mod se m_alias env ->
  let env =
    { env with avail_defs = []; parent = env.avail_defs @ env.parent; parent_mod }
  in
  let m_alias, defs_or_alias_opt, env =
    match se.wrap_content with
    (* Signature alias: *)
    | S_sig { items = [ S_include { wrap_content = S_path path; location = _ } ] } ->
      let m_alias, alias_opt = resolve_module_alias parent_mod path env m_alias in
      m_alias, alias_opt, env
    | S_sig decls ->
      let m_alias, env = signature decls m_alias env in
      let alias_opt = Some (Defs env.avail_defs) in
      m_alias, alias_opt, env
    | S_path path ->
      let m_alias, alias_opt = resolve_module_alias parent_mod path env m_alias in
      m_alias, alias_opt, env
  in
  m_alias, defs_or_alias_opt, env.module_map


(** [signature_expression_from_annotation] is like [signature_expression] but with similar
    considerations of [Definitions.mod_cases_of_sig_expr_from_annotation]. *)
and signature_expression_from_annotation : AST.signature_expr -> t -> env -> t * env =
 fun se m_alias old_env ->
  let env =
    { old_env with avail_defs = []; parent = old_env.avail_defs @ old_env.parent }
  in
  let m_alias, env =
    match se.wrap_content with
    | S_sig { items } ->
      List.fold items ~init:(m_alias, env) ~f:(fun (m_alias, env) -> function
        | S_include { wrap_content = S_sig sig'; location = _ } ->
          let m_alias, env' = signature sig' m_alias env in
          m_alias, { env' with module_map = env.module_map }
        | S_include { wrap_content = S_path _path; location = _ } -> m_alias, env
        | S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _ ->
          failwith
          @@ Format.asprintf
               "signature_expression_from_annotation: corner case reached: %a"
               AST.PP.signature_expr
               se)
    | S_path path -> m_alias, env
  in
  m_alias, { old_env with module_map = env.module_map }


(** [signature] builds the [env] and tries to resolves module aliases *)
and signature : AST.signature -> t -> env -> t * env =
 fun { items } m_alias env ->
  List.fold items ~init:(m_alias, env) ~f:(fun (m_alias, env) item ->
      sig_item item m_alias env)


(** [sig_item] builds the [env] and tries to resolves module aliases *)
and sig_item : AST.sig_item -> t -> env -> t * env =
 fun si m_alias env ->
  match si with
  | S_value _ | S_type _ | S_type_var _ -> m_alias, env
  | S_module (var, sig') | S_module_type (var, sig') ->
    let m_alias, env = signature sig' m_alias env in
    m_alias, env
  | S_include sig_expr ->
    let m_alias, env = signature_expression sig_expr m_alias env in
    m_alias, env


(** [module_annotation] builds the [env] and tries to resolves module aliases *)
and module_annotation : AST.module_annotation -> t -> env -> t * env =
 fun { signature; filter = _ } -> signature_expression_from_annotation signature


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
  | D_module { module_binder; module_; module_attr = _; annotation } ->
    let m_alias, env =
      Option.value_map annotation ~default:(m_alias, env) ~f:(fun annotation ->
          module_annotation annotation m_alias env)
    in
    let m_alias, defs_or_alias_opt, module_map =
      module_expression (Some module_binder) module_ m_alias env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    m_alias, env
  | D_signature { signature_binder; signature; signature_attr = _ } ->
    let m_alias, defs_or_alias_opt, module_map =
      signature_expression_from_D_signature (Some signature_binder) signature m_alias env
    in
    let env = Env.add_mvar signature_binder defs_or_alias_opt module_map env in
    m_alias, env
  | D_module_include mod_expr ->
    let m_alias, defs_or_alias_opt, module_map =
      module_expression env.parent_mod mod_expr m_alias env
    in
    let env = Env.include_mvar defs_or_alias_opt module_map env in
    m_alias, env


(** [declarations] builds the [env] and tries to resolves module aliases *)
and declarations : AST.declaration list -> t -> env -> t * env =
 fun decls m_alias env ->
  List.fold decls ~init:(m_alias, env) ~f:(fun (m_alias, env) decl ->
      declaration decl m_alias env)


(** [declarations] sets up the initial env and calls [declarations] *)
let declarations : AST.declaration list -> t * env =
 fun decls ->
  let m_alias = LMap.empty in
  let env = Env.empty in
  declarations decls m_alias env


(** [patch] fixes the module aliases in the [defs]. It looks for module aliases
    definitions & then looks up the range of the module definition in [t] *)
let rec patch : t -> Types.def list -> Types.def list =
 fun m_alias ->
  let open Types in
  (* Find the largest location smaller than or equal to the path that matches the name. *)
  (* TODO: the resolved module path might not match what we expect *)
  let find_uid : Uid.t -> (Uid.t list * Uid.t) option =
   fun uid ->
    let name = Uid.to_name uid in
    let loc = Uid.to_location uid in
    let same_name_lte =
      LMap.filter
        (fun _loc (_resolved_module_path, resolved_module) ->
          Location_ordered.compare (Uid.to_location resolved_module) loc <= 0
          && String.equal (Uid.to_name resolved_module) name)
        m_alias
    in
    Option.map ~f:snd @@ LMap.max_binding_opt same_name_lte
  in
  let patch_resolve_mod_name path =
    let module_path = get_module_path path in
    match
      let%bind.Option uid = List.last module_path in
      find_uid uid
    with
    | None -> path
    | Some (resolved_module_path, resolved_module) ->
      Resolved_path { module_path; resolved_module_path; resolved_module }
  in
  let patch_implementation = function
    | Ad_hoc_signature defs -> Ad_hoc_signature (patch m_alias defs)
    | Standalone_signature_or_module path ->
      Standalone_signature_or_module (patch_resolve_mod_name path)
  in
  let patch_extension = patch_resolve_mod_name in
  List.map ~f:(function
      | Module m ->
        let patch_mod_case = function
          | Def defs -> Def (patch m_alias defs)
          | Alias { resolve_mod_name; file_name } as alias ->
            (match LMap.find_opt m.range m_alias with
            | None -> alias
            | Some (resolved_module_path, resolved_module) ->
              Alias
                { resolve_mod_name =
                    Resolved_path
                      { module_path = get_module_path resolve_mod_name
                      ; resolved_module_path
                      ; resolved_module
                      }
                ; file_name
                })
        in
        Module
          { m with
            mod_case = patch_mod_case m.mod_case
          ; implements = List.map ~f:patch_implementation m.implements
          ; extends = List.map ~f:patch_extension m.extends
          }
      | (Variable _ | Type _) as def -> def)
