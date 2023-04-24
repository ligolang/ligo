open Ligo_prim
open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap = Map.Make (Location_ordered)
open Env

(* --------------------------- Module usage --------------------------------- *)

(** [module_usages] is an associative list where key the [Module_var.t] of the 
    original defintion of a module & the value is the usage [Location.t].
    
    This is helpful when updating the references of a [E_module_accessor], 
    [M_variable] & [M_module_path]

    e.g. module A = C.D
    In the module expression C.D the [module_usages] will look something like
    [ (C.1, `Usage (the location of C in module_expression))
    ; (D.2, `Usage (the location of D in module_expression))] *)
type module_usage = Module_var.t * [ `Usage of Location.t ]

type module_usages = module_usage list

module References = struct
  type references = LSet.t LMap.t
  type t = references

  (** Updates the references map, adding [usgae_loc] to the list of locations mentioning [def]. *)
  let add_def_usage : def:def -> usage_loc:Location.t -> references -> references =
   fun ~def ~usage_loc refs ->
    LMap.update
      (Def.get_location def)
      (function
        | None -> Some (LSet.singleton usage_loc)
        | Some locs -> Some (LSet.add usage_loc locs))
      refs


  (** Wrapper over [add_def_usage].
      If the def option is [Some def], calls [add_def_usage] with [def].
      Else, leave the reference map untouched *)
  let add_def_opt_usage : Location.t -> def option -> references -> references =
   fun usage_loc def_opt refs ->
    match def_opt with
    | Some def -> add_def_usage ~def ~usage_loc refs
    | None -> refs


  (** [update_vvar_reference] looks up the [Value_var.t] in the [env] and updates
      the [references] with the help of [add_def_opt_usage] *)
  let add_vvar : Value_var.t -> env -> references -> references =
   fun v env refs ->
    let def_opt = Env.lookup_vvar_opt env v in
    add_def_opt_usage (Value_var.get_location v) def_opt refs


  (** [update_tvar_reference] looks up the [Type_var.t] in the [env] and updates
      the [references] with the help of [add_def_opt_usage] *)
  let add_tvar : Type_var.t -> env -> references -> references =
   fun t env refs ->
    let def_opt = Env.lookup_tvar_opt env t in
    add_def_opt_usage (Type_var.get_location t) def_opt refs


  (* TODO : Shouldn't this be the same as the two above ?? *)

  (** [update_mvar_references] takes [module_usages] and updates the [references]
      for each [Module_var.t] it adds a usage [Location.t] to it's [references] *)
  let add_module_usages : module_usages -> references -> references =
   fun module_usages references ->
    let add_module_usage : references -> module_usage -> references =
     fun refs (orig, `Usage use) -> add_def_usage ~def:(Module orig) ~usage_loc:use refs
    in
    List.fold module_usages ~init:references ~f:add_module_usage


  (* List.fold module_usages ~init:references ~f:(fun refs (orig, `Usage use) ->
        let orig = Module_var.get_location orig in
        LMap.update
          orig
          (function
            | None -> Some (LSet.singleton use)
            | Some locs -> Some (LSet.add use locs))
          refs) *)

  (** [add_maccess] is responsible for updating the [references]
      for a module access e.g. let x = M.N.o
      1. In case of an Alias, it will look up the rhs and recursively 
        resolve it in the [module_map]
      2. In case of a Def, it call the callback funtion [update_element_reference] to
        updateh the reference of [element].
      *)

  (* let rec add_maccess
      :  update_element_reference:('a -> env -> references -> references) -> 'a
      -> Module_var.t -> module_map -> references -> references
    =
   fun ~update_element_reference element mv module_map refs ->
    let self mv = add_maccess element mv module_map refs ~update_element_reference in
    match Module_map.find_opt mv module_map with
    | Some (Defs defs) ->
      update_element_reference element { parent = []; avail_defs = defs; module_map } refs
    | Some (Alias mv) -> self mv
    | None -> refs *)

  let add_maccess
      :  update_element_reference:('a -> env -> references -> references) -> 'a
      -> Module_var.t -> module_map -> references -> references
    =
   fun ~update_element_reference element mv module_map refs ->
    match Module_map.resolve_mvar mv module_map with
    | Some (_orig, defs) ->
      update_element_reference element { parent = []; avail_defs = defs; module_map } refs
    | None -> refs


  (** [update_references_for_module_access_var] updates refences of a module accessed value/var *)
  let add_maccess_vvar
      : Value_var.t -> Module_var.t -> module_map -> references -> references
    =
   fun v -> add_maccess ~update_element_reference:add_vvar v


  (** [update_references_for_module_access_type] updates refences of a module accessed type *)
  let add_maccess_tvar
      : Type_var.t -> Module_var.t -> module_map -> references -> references
    =
   fun t -> add_maccess ~update_element_reference:add_tvar t
end

type references = References.t

(** [resolve_module_alias_in_env] takes a module path ([Module_var.t List.Ne.t]) 
    & [env] and does two things,
    1. It takes a module path, like A.B.C, and returns the actual module it points to.
      It proceeds element by element, resolving A first :
      - if A is an alias to A', it looks for what A' points to.
      - if A is an actual module, it takes its declaration list and looks for B in it, 
        then recursively resolves B
      It does this on A, then B, then C. And returns the actual module pointed to by C.
    2. During the resolution of module path, it accumulates the actual module names & 
      the location where they are used and returns [module_usages]. *)

(* let resolve_mpath
    :  Module_var.t List.Ne.t -> env
    -> (Module_var.t * module_usages, module_usages) result
  =
 fun mvs env ->
  let rec aux
      :  Module_var.t List.Ne.t -> def list -> module_usages
      -> (Module_var.t * module_usages, module_usages) result
    =
   fun (mv, mvs) defs acc_mvs ->
    let mv_loc = Module_var.get_location mv in
    match Env.resolve_mvar mv defs env.module_map with
    | None ->
      (* Module var not found module *)
      Error acc_mvs
    | Some (real, resolved, defs_of_that_module) ->
      (match mvs with
      | [] ->
        (* Terminal call, no nested-module to lookup *)
        Ok (resolved, (real, `Usage mv_loc) :: acc_mvs)
      | mv' :: mvs -> aux (mv', mvs) defs_of_that_module ((real, `Usage mv_loc) :: acc_mvs))
  in
  aux mvs (env.avail_defs @ env.parent) [] *)

let resolve_mpath
    :  Module_var.t List.Ne.t -> env
    -> (Module_var.t * module_usages, module_usages) result
  =
 fun mvs env ->
  let init : module_usages = [] in
  let f : module_usages -> _ -> module_usages =
   fun acc (input, real, _resolved, _defs_of_that_module) ->
    let input_loc = Module_var.get_location input in
    (real, `Usage input_loc) :: acc
  in
  let defs = env.avail_defs @ env.parent in
  let mmap = env.module_map in
  let acc, m_opt = Env.fold_resolve_mpath mvs defs mmap ~init ~f in
  match m_opt with
  | None -> Error acc
  | Some (_real, resolved, _defs) -> Ok (resolved, acc)


(** [resolve_module_alias_and_update_mvar_references] will resolve the module path 
    with the help of [resolve_module_alias_in_env] and in addition to that It'll 
    update the references of all the module path elements. *)
let resolve_module_alias_and_update_mvar_references
    :  ?update_references_for_element:
         ('a -> Module_var.t -> module_map -> references -> references)
    -> ?element:'a -> Module_var.t List.Ne.t -> env -> references
    -> references * defs_or_alias option * env
  =
 fun ?(update_references_for_element = fun _ _ _ rs -> rs) ?element mvs env refs ->
  let ma_res = resolve_mpath mvs env in
  match ma_res with
  | Ok (ma, module_usages) ->
    let refs = References.add_module_usages module_usages refs in
    let refs =
      Option.value_map element ~default:refs ~f:(fun element ->
          update_references_for_element element ma env.module_map refs)
    in
    refs, Some (Alias ma), env
  | Error module_usages ->
    let refs = References.add_module_usages module_usages refs in
    refs, None, env


(* --------------------------- AST traversal -------------------------------- *)

(** [expression] take an [AST.expression] & traverses its sub-expressions and
    upates the [references] & [env] *)
let rec expression : AST.expression -> references -> env -> references =
 fun e refs env ->
  match e.expression_content with
  | E_variable v -> References.add_vvar v env refs
  | E_module_accessor { module_path; element } ->
    let refs, _, _ =
      resolve_module_alias_and_update_mvar_references
        (List.Ne.of_list module_path)
        env
        refs
        ~element
        ~update_references_for_element:References.add_maccess_vvar
    in
    refs
  | E_literal _ -> refs
  | E_constant { arguments; cons_name = _ } ->
    List.fold arguments ~init:refs ~f:(fun refs arg -> expression arg refs env)
  | E_application { lamb; args } ->
    let refs = expression lamb refs env in
    expression args refs env
  | E_lambda { binder; output_type; result } ->
    let refs =
      Option.fold (Param.get_ascr binder) ~init:refs ~f:(fun refs t ->
          type_expression t refs env)
    in
    let refs =
      Option.fold output_type ~init:refs ~f:(fun refs output_type ->
          type_expression output_type refs env)
    in
    let env = Env.add_vvar (Param.get_var binder) env in
    expression result refs env
  | E_recursive
      { fun_name
      ; fun_type
      ; lambda = { binder; output_type; result }
      ; force_lambdarec = _
      } ->
    let refs = type_expression fun_type refs env in
    let refs = type_expression output_type refs env in
    let refs = type_expression (Param.get_ascr binder) refs env in
    let env = env |> Env.add_vvar fun_name |> Env.add_vvar (Param.get_var binder) in
    expression result refs env
  | E_type_abstraction { type_binder = _; result } -> expression result refs env
  | E_let_mut_in { let_binder; rhs; let_result; attributes = _ }
  | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
    let binders = Linear_pattern.binders let_binder in
    let vars = List.map binders ~f:Binder.get_var in
    let refs = expression rhs refs env in
    let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
    expression let_result refs env
  | E_type_in { type_binder; rhs; let_result } ->
    let refs = type_expression rhs refs env in
    let env = Env.add_tvar type_binder env in
    expression let_result refs env
  | E_raw_code { language = _; code } -> expression code refs env
  | E_constructor { constructor = _; element } -> expression element refs env
  | E_matching { matchee; cases } ->
    let refs = expression matchee refs env in
    List.fold cases ~init:refs ~f:(fun refs { pattern; body } ->
        let binders = Linear_pattern.binders pattern in
        let vars = List.map binders ~f:Binder.get_var in
        let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
        expression body refs env)
  | E_record e_label_map ->
    let es = Record.values e_label_map in
    List.fold es ~init:refs ~f:(fun refs e -> expression e refs env)
  | E_accessor { struct_; path = _ } -> expression struct_ refs env
  | E_update { struct_; path = _; update } ->
    let refs = expression struct_ refs env in
    expression update refs env
  | E_ascription { anno_expr; type_annotation } ->
    let refs = type_expression type_annotation refs env in
    expression anno_expr refs env
  | E_assign { binder; expression = e } ->
    let refs = References.add_vvar (Binder.get_var binder) env refs in
    expression e refs env
  | E_for { binder; start; final; incr; f_body } ->
    let refs = expression start refs env in
    let refs = expression final refs env in
    let refs = expression incr refs env in
    let env = Env.add_vvar binder env in
    expression f_body refs env
  | E_for_each { fe_binder = v, v_opt; collection; collection_type = _; fe_body } ->
    let refs = expression collection refs env in
    let env = Env.add_vvar v env in
    let env = Option.fold v_opt ~init:env ~f:(Fn.flip Env.add_vvar) in
    expression fe_body refs env
  | E_while { cond; body } ->
    let refs = expression cond refs env in
    expression body refs env
  | E_mod_in { module_binder; rhs; let_result } ->
    let refs, defs_or_alias_opt, module_map = module_expression rhs refs env in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    expression let_result refs env


(** [module_expression] takes a [AST.type_expression] it uses the [env] to 
    update the [references]  *)
and type_expression : AST.type_expression -> references -> env -> references =
 fun te refs env ->
  match te.type_content with
  | T_variable tv -> References.add_tvar tv env refs
  | T_module_accessor { module_path; element } ->
    let refs, _, _ =
      resolve_module_alias_and_update_mvar_references
        (List.Ne.of_list module_path)
        env
        refs
        ~element
        ~update_references_for_element:References.add_maccess_tvar
    in
    refs
  | T_app { type_operator = { module_path; element }; arguments } ->
    let refs =
      match module_path with
      | [] -> References.add_tvar element env refs
      | module_path ->
        let refs, _, _ =
          resolve_module_alias_and_update_mvar_references
            (List.Ne.of_list module_path)
            env
            refs
            ~element
            ~update_references_for_element:References.add_maccess_tvar
        in
        refs
    in
    List.fold arguments ~init:refs ~f:(fun refs te -> type_expression te refs env)
  | T_singleton _ -> refs
  | T_sum { layout = _; fields } | T_record { layout = _; fields } ->
    Record.fold fields ~init:refs ~f:(fun refs te -> type_expression te refs env)
  | T_arrow { type1; type2 } ->
    let refs = type_expression type1 refs env in
    type_expression type2 refs env
  | T_for_all { ty_binder = _; kind = _; type_ }
  | T_abstraction { ty_binder = _; kind = _; type_ } -> type_expression type_ refs env


(** [module_expression] takes a [AST.module_expr] and depending on the type of 
    module i.e. alias or struct returns [defs_or_alias] 
     - for a module alias (e.g. module A = B.C.D) it resolves B.C.D to what D points
       to. In case of an alias, it will further resolve until it finds an actual 
       module
     - for a actual module, it returns the list of its [def]'s
    and updates the [references] & the [module_map] *)
and module_expression
    :  AST.module_expr -> references -> env
    -> references * defs_or_alias option * module_map
  =
 fun me refs env ->
  (* Move [avail_defs] to [parent] before finding [references] in module_expr *)
  let env = { env with avail_defs = []; parent = env.avail_defs @ env.parent } in
  let refs, defs_or_alias_opt, env =
    match me.wrap_content with
    | M_struct decls ->
      let refs, env = declarations decls refs env in
      refs, Some (Defs env.avail_defs), env
    | M_variable mv ->
      let refs, alias_opt, env =
        resolve_module_alias_and_update_mvar_references (mv, []) env refs
      in
      refs, alias_opt, env
    | M_module_path mvs ->
      let refs, alias_opt, env =
        resolve_module_alias_and_update_mvar_references mvs env refs
      in
      refs, alias_opt, env
  in
  refs, defs_or_alias_opt, env.module_map


(** [declaration] takes [AST.declaration] and updates the [references] & [env] *)
and declaration : AST.declaration -> references -> env -> references * env =
 fun d refs env ->
  match d.wrap_content with
  | D_value { binder; expr; attr = _ } ->
    let refs =
      Option.fold (Binder.get_ascr binder) ~init:refs ~f:(fun refs t ->
          type_expression t refs env)
    in
    let var = Binder.get_var binder in
    let refs = expression expr refs env in
    let env = Env.add_vvar var env in
    refs, env
  | D_irrefutable_match { pattern; expr; attr = _ } ->
    let binder = Linear_pattern.binders pattern in
    let tys = List.map binder ~f:Binder.get_ascr in
    let refs =
      List.fold tys ~init:refs ~f:(fun refs ty_opt ->
          match ty_opt with
          | None -> refs
          | Some ty -> type_expression ty refs env)
    in
    let vars = List.map binder ~f:Binder.get_var in
    let refs = expression expr refs env in
    let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
    refs, env
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let refs = type_expression type_expr refs env in
    let env = Env.add_tvar type_binder env in
    refs, env
  | D_module { module_binder; module_; module_attr = _ } ->
    let refs, defs_or_alias_opt, module_map = module_expression module_ refs env in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    refs, env


(** [declarations] takes a list of [AST.declaration], [references] & [env]
    and for each delcaration it calls the [delcaration] function with updates
    the [references] & [env] *)
and declarations : AST.declaration list -> references -> env -> references * env =
 fun decls refs env ->
  let refs, env =
    List.fold decls ~init:(refs, env) ~f:(fun (refs, env) decl ->
        declaration decl refs env)
  in
  refs, env


let declarations : AST.declaration list -> references =
 fun decls ->
  let refs = LMap.empty in
  let env = Env.empty in
  let refs, _ = declarations decls refs env in
  refs


let rec patch : references -> Types.def list -> Types.def list =
 fun refs defs ->
  List.map defs ~f:(fun def ->
      match def with
      | Variable v ->
        (match LMap.find_opt v.range refs with
        | None -> Types.Variable v
        | Some references -> Variable { v with references })
      | Type t ->
        (match LMap.find_opt t.range refs with
        | None -> Types.Type t
        | Some references -> Type { t with references })
      | Module m ->
        let mod_case =
          match m.mod_case with
          | Alias a -> Types.Alias a
          | Def defs -> Def (patch refs defs)
        in
        let m =
          match LMap.find_opt m.range refs with
          | None -> m
          | Some references -> { m with references }
        in
        Module { m with mod_case })
