open Ligo_prim
open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap = Map.Make (Location)

(** The Module_map maps modules to their aliasee (in case of module alias) or 
    list of defs (in case of mod expr).
    Two different modules will always correspond to two different entries.
    However, [Module_var.compare] doesn't account for location, so same-name
    modules will wrongly correspond to the same key, so same entry. 
    This is why we augment [Module_var] with a location-aware comparison function. *)
module Module_map = Map.Make (struct
  type t = Module_var.t

  let compare m1 m2 =
    match Module_var.compare m1 m2 with
    | 0 -> Location.compare (Module_var.get_location m1) (Module_var.get_location m2)
    | cmp -> cmp
end)

type def =
  | Variable of Value_var.t
  | Type of Type_var.t
  | Module of Module_var.t

let extract_location = function
  | Variable v -> Value_var.get_location v
  | Type t -> Type_var.get_location t
  | Module m -> Module_var.get_location m


type defs_or_alias =
  | Defs of def list
  | Alias of Module_var.t

type references = LSet.t LMap.t
type module_map = defs_or_alias Module_map.t

(** In [env] the record fields represent the following:
    1. [parent] represents the defintions from the parent(ancestors) modules.
    2. [avail_defs] represents the top-level defintions of the current module
        & in the context of an expression it represents the bindings encountered 
        sofar.
    3. [module_map] is a map from [Module_var.t] -> [defs_or_alias] this is a flat
        representation of modules & its contents, this simplifies the handling
        for nested modules *)
type env =
  { parent : def list
  ; avail_defs : def list
  ; module_map : module_map
  }

(** [module_usage] is an associative list where key the [Module_var.t] of the 
    original defintion of a module & the value is the usage [Location.t].
    
    This is helpful when updating the references of a [E_module_accessor], 
    [M_variable] & [M_module_path]

    e.g. module A = C.D
    In the module expression C.D the [module_usages] will look something like
    [ (C.1, `Usage (the location of C in module_expression))
    ; (D.2, `Usage (the location of D in module_expression))] *)
type module_usage = (Module_var.t * [ `Usage of Location.t ]) list

let empty_env = { parent = []; avail_defs = []; module_map = Module_map.empty }

(** [find_definition_location] is a helper function which will look up the [env]
    and try to find the [Location.t] of the [def]
    It first tries to look up the [avail_defs] then if it does not find the [def]
    there it looks up the [parent] defs *)
let find_definition_opt : env -> (def -> bool) -> def option =
 fun env f ->
  let find (defs : def list) = List.find ~f defs in
  match find env.avail_defs with
  | Some def -> Some def
  | None -> find env.parent


(** [vvar_definition_location] looks up the [Value_var.t] in the [env] with the
    help of [find_definition_location] *)
let vvar_definition_location : env -> Value_var.t -> def option =
 fun env v ->
  find_definition_opt env (function
      | Variable v' -> Value_var.equal v v'
      | Type _ | Module _ -> false)


(** [tvar_definition_location] looks up the [Type_var.t] in the [env] with the
    help of [find_definition_location] *)
let tvar_definition_location : env -> Type_var.t -> def option =
 fun env t ->
  find_definition_opt env (function
      | Type t' -> Type_var.equal t t'
      | Variable _ | Module _ -> false)


(** [add_vvar_to_avail_defs] adds [Value_var.t] to [avail_defs] in the [env] *)
let add_vvar_to_avail_defs : Value_var.t -> env -> env =
 fun v env -> { env with avail_defs = Variable v :: env.avail_defs }


(** [add_tvar_to_avail_defs] adds [Type_var.t] to [avail_defs] in the [env] *)
let add_tvar_to_avail_defs : Type_var.t -> env -> env =
 fun t env -> { env with avail_defs = Type t :: env.avail_defs }


(** [update_reference] is a helper function the take a usage [Location.t] &
    an optional [def] and tries the update the [references] map *)
let update_reference : Location.t -> def option -> references -> references =
 fun usage_loc def_opt refs ->
  match def_opt with
  | Some def ->
    LMap.update
      (extract_location def)
      (function
        | None -> Some (LSet.singleton usage_loc)
        | Some locs -> Some (LSet.add usage_loc locs))
      refs
  | None -> refs


(** [update_vvar_reference] looks up the [Value_var.t] in the [env] and updates
    the [references] with the help of [update_reference] *)
let update_vvar_reference : Value_var.t -> env -> references -> references =
 fun v env refs ->
  let def_loc_opt = vvar_definition_location env v in
  update_reference (Value_var.get_location v) def_loc_opt refs


(** [update_tvar_reference] looks up the [Type_var.t] in the [env] and updates
    the [references] with the help of [update_reference] *)
let update_tvar_reference : Type_var.t -> env -> references -> references =
 fun t env refs ->
  let def_loc_opt = tvar_definition_location env t in
  update_reference (Type_var.get_location t) def_loc_opt refs


(** [update_mvar_references] takes [module_usages] and updates the [references]
    for each [Module_var.t] it adds a usage [Location.t] to it's [references] *)
let update_mvar_references : module_usage -> references -> references =
 fun module_usages references ->
  List.fold module_usages ~init:references ~f:(fun refs (orig, `Usage use) ->
      let orig = Module_var.get_location orig in
      LMap.update
        orig
        (function
          | None -> Some (LSet.singleton use)
          | Some locs -> Some (LSet.add use locs))
        refs)


(** [add_module_to_env] adds [Module_var.t] to the [avail_defs] and also adds an
    [defs_or_alias] to the [module_map] in the [env] *)
let add_module_to_env : Module_var.t -> defs_or_alias option -> module_map -> env -> env =
 fun m defs_or_alias_opt module_map env ->
  let env = { env with module_map } in
  Option.fold defs_or_alias_opt ~init:env ~f:(fun env defs_or_alias ->
      { env with
        avail_defs = Module m :: env.avail_defs
      ; module_map = Module_map.add m defs_or_alias env.module_map
      })


(** [find_in_defs] tries to find the defintion of the [Module_var.t] in the [defs] *)
let find_in_defs : Module_var.t -> def list -> Module_var.t option =
 fun m defs ->
  List.find defs ~f:(function
      | Variable _ | Type _ -> false
      | Module m' -> Module_var.equal m m')
  |> Option.find_map ~f:(function
         | Module m -> Some m
         | Variable _ | Type _ -> None)


(** [resolve_module_alias_in_env] takes a module path ([Module_var.t List.Ne.t]) 
    & [env] and does two things,
    1. It takes a module path, like A.B.C, and returns the actual module it points to.
       It proceeds element by element, resolving A first :
       - if A is an alias to A', it looks for what A' points to.
       - if A is an actual module, it takes its declaration list and looks for B in it, 
         then recursively resolves B
       It does this on A, then B, then C. And returns the actual module pointed to by C.
    2. During the resolution of module path, it accumulates the actual module names & 
       the location where they are used and returns [module_usage]. *)
let resolve_module_alias_in_env
    : Module_var.t List.Ne.t -> env -> (Module_var.t * module_usage, module_usage) result
  =
 fun mvs env ->
  let module_map = env.module_map in
  (** [resolve_in_map] takes [Module_var.t] and tries to resolve it in the [module_map],
      in case of module alias it recusively resolves the alias in the [module_map]. *)
  let rec resolve_in_map : Module_var.t -> (Module_var.t * def list) option =
   fun mv ->
    match Module_map.find_opt mv module_map with
    | Some (Defs defs) -> Some (mv, defs)
    | Some (Alias mv') -> resolve_in_map mv'
    | None -> None
  in
  (** [resolve_module_var] a [Module_var.t] and looks up in the defs ([def list])
      it gets the module var, and then calls [resolve_in_map] for module var *)
  let resolve_module_var
      :  Module_var.t -> def list
      -> (Module_var.t * [ `Resolved of Module_var.t ] * def list) option
    =
   fun mv defs ->
    (* First look in the avail defs *)
    match find_in_defs mv defs with
    | None -> None
    | Some real ->
      (* Then look in the map *)
      (match resolve_in_map real with
      | None -> None
      | Some (mv, defs) -> Some (real, `Resolved mv, defs))
  in
  let rec aux
      :  Module_var.t List.Ne.t -> def list -> module_usage
      -> (Module_var.t * module_usage, module_usage) result
    =
   fun mvs defs acc_mvs ->
    let mv, mvs = mvs in
    let mv_loc = Module_var.get_location mv in
    match resolve_module_var mv defs with
    | None ->
      (* Module var not found module *)
      Error acc_mvs
    | Some (real, `Resolved resolved, defs_of_that_module) ->
      (match mvs with
      | [] ->
        (* Terminal call, no nested-module to lookup *)
        Ok (resolved, (real, `Usage mv_loc) :: acc_mvs)
      | mv' :: mvs -> aux (mv', mvs) defs_of_that_module ((real, `Usage mv_loc) :: acc_mvs))
  in
  aux mvs (env.avail_defs @ env.parent) []

(** [update_references_for_module_access] is responsible for updating the [references]
    for a module access e.g. let x = M.N.o
    1. In case of an Alias, it will look up the rhs and recursively 
       resolve it in the [module_map]
    2. In case of a Def, it call the callback funtion [update_element_reference] to
       updateh the reference of [element].
    *)
let rec update_references_for_module_access
    :  update_element_reference:('a -> env -> references -> references) -> 'a
    -> Module_var.t -> module_map -> references -> references
  =
 fun ~update_element_reference element mv module_map refs ->
  match Module_map.find_opt mv module_map with
  | Some (Defs defs) ->
    update_element_reference element { parent = []; avail_defs = defs; module_map } refs
  | Some (Alias mv) ->
    update_references_for_module_access
      element
      mv
      module_map
      refs
      ~update_element_reference
  | None -> refs

(** [update_references_for_module_access_var] updates refences of a module accessed value/var *)
let update_references_for_module_access_var
    : Value_var.t -> Module_var.t -> module_map -> references -> references
  =
 fun v mv module_map refs ->
  update_references_for_module_access
    ~update_element_reference:update_vvar_reference
    v
    mv
    module_map
    refs

(** [update_references_for_module_access_type] updates refences of a module accessed type *)
let update_references_for_module_access_type
    : Type_var.t -> Module_var.t -> module_map -> references -> references
  =
 fun t mv module_map refs ->
  update_references_for_module_access
    ~update_element_reference:update_tvar_reference
    t
    mv
    module_map
    refs

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
  let ma_res = resolve_module_alias_in_env mvs env in
  match ma_res with
  | Ok (ma, module_usages) ->
    let refs = update_mvar_references module_usages refs in
    let refs =
      Option.value_map element ~default:refs ~f:(fun element ->
          update_references_for_element element ma env.module_map refs)
    in
    refs, Some (Alias ma), env
  | Error module_usages ->
    let refs = update_mvar_references module_usages refs in
    refs, None, env


(** [expression] take an [AST.expression] & traverses its sub-expressions and
    upates the [references] & [env] *)
let rec expression : AST.expression -> references -> env -> references =
 fun e refs env ->
  match e.expression_content with
  | E_variable v -> update_vvar_reference v env refs
  | E_module_accessor { module_path; element } ->
    let refs, _, _ =
      resolve_module_alias_and_update_mvar_references
        (List.Ne.of_list module_path)
        env
        refs
        ~element
        ~update_references_for_element:update_references_for_module_access_var
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
    let env = add_vvar_to_avail_defs (Param.get_var binder) env in
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
    let env =
      env
      |> add_vvar_to_avail_defs fun_name
      |> add_vvar_to_avail_defs (Param.get_var binder)
    in
    expression result refs env
  | E_type_abstraction { type_binder = _; result } -> expression result refs env
  | E_let_mut_in { let_binder; rhs; let_result; attributes = _ }
  | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
    let binders = Linear_pattern.binders let_binder in
    let vars = List.map binders ~f:Binder.get_var in
    let refs = expression rhs refs env in
    let env = List.fold_right vars ~init:env ~f:add_vvar_to_avail_defs in
    expression let_result refs env
  | E_type_in { type_binder; rhs; let_result } ->
    let refs = type_expression rhs refs env in
    let env = add_tvar_to_avail_defs type_binder env in
    expression let_result refs env
  | E_raw_code { language = _; code } -> expression code refs env
  | E_constructor { constructor = _; element } -> expression element refs env
  | E_matching { matchee; cases } ->
    let refs = expression matchee refs env in
    List.fold cases ~init:refs ~f:(fun refs { pattern; body } ->
        let binders = Linear_pattern.binders pattern in
        let vars = List.map binders ~f:Binder.get_var in
        let env = List.fold_right vars ~init:env ~f:add_vvar_to_avail_defs in
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
    let refs = update_vvar_reference (Binder.get_var binder) env refs in
    expression e refs env
  | E_for { binder; start; final; incr; f_body } ->
    let refs = expression start refs env in
    let refs = expression final refs env in
    let refs = expression incr refs env in
    let env = add_vvar_to_avail_defs binder env in
    expression f_body refs env
  | E_for_each { fe_binder = v, v_opt; collection; collection_type = _; fe_body } ->
    let refs = expression collection refs env in
    let env = add_vvar_to_avail_defs v env in
    let env = Option.fold v_opt ~init:env ~f:(Fn.flip add_vvar_to_avail_defs) in
    expression fe_body refs env
  | E_while { cond; body } ->
    let refs = expression cond refs env in
    expression body refs env
  | E_mod_in { module_binder; rhs; let_result } ->
    let refs, defs_or_alias_opt, module_map = module_expression rhs refs env in
    let env = add_module_to_env module_binder defs_or_alias_opt module_map env in
    expression let_result refs env


(** [module_expression] takes a [AST.type_expression] it uses the [env] to 
    update the [references]  *)
and type_expression : AST.type_expression -> references -> env -> references =
 fun te refs env ->
  match te.type_content with
  | T_variable tv -> update_tvar_reference tv env refs
  | T_module_accessor { module_path; element } ->
    let refs, _, _ =
      resolve_module_alias_and_update_mvar_references
        (List.Ne.of_list module_path)
        env
        refs
        ~element
        ~update_references_for_element:update_references_for_module_access_type
    in
    refs
  | T_app { type_operator = { module_path; element }; arguments } ->
    let refs =
      match module_path with
      | [] -> update_tvar_reference element env refs
      | module_path ->
        let refs, _, _ =
          resolve_module_alias_and_update_mvar_references
            (List.Ne.of_list module_path)
            env
            refs
            ~element
            ~update_references_for_element:update_references_for_module_access_type
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
    let env = add_vvar_to_avail_defs var env in
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
    let env = List.fold_right vars ~init:env ~f:add_vvar_to_avail_defs in
    refs, env
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let refs = type_expression type_expr refs env in
    let env = add_tvar_to_avail_defs type_binder env in
    refs, env
  | D_module { module_binder; module_; module_attr = _ } ->
    let refs, defs_or_alias_opt, module_map = module_expression module_ refs env in
    let env = add_module_to_env module_binder defs_or_alias_opt module_map env in
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
  let env = empty_env in
  let refs, _ = declarations decls refs env in
  refs
