open Ligo_prim
module Location = Simple_utils.Location
open Simple_utils.Function
module AST = Ast_core
module LSet = Types.LSet
module LMap = Types.LMap
open Env
open Env_map
module Env = Env_map

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

(** A list of [module_usage]s. *)
type module_usages = module_usage list

module References = struct
  (** Associates a location indicating some definition's range to a set of locations which
      are all of its references (usages). *)
  type references = LSet.t LMap.t

  (** A map from original type variable location to labels with their locations.
      Take this example:
      {[
        type t = Foo | Bar of int

        let x = (Foo : t)
        let y = ((Bar 42) : t)
        let z = ((Bar 24) : t)
      ]}
      Here we'll have a mapping from [t] location to labels [Foo] and [Bar].
      [Bar] label will have two locations in its set and [Foo] only one. *)
  type type_var_loc_to_label_refs = LSet.t Label.Map.t LMap.t

  (** The result of running this pass. *)
  type t =
    { references : references
    ; type_var_loc_to_label_refs : type_var_loc_to_label_refs
    }

  type label_types = Ast_core.ty_expr LMap.t

  (** For debugging. *)
  let[@warning "-32"] pp : t Fmt.t =
   fun ppf t ->
    Fmt.Dump.(list (pair Location.pp (fun ppf -> list Location.pp ppf <@ Set.to_list)))
      ppf
    @@ Map.to_alist t.references


  let map_ref f t = { t with references = f t.references }
  let map_tv f t = { t with type_var_loc_to_label_refs = f t.type_var_loc_to_label_refs }

  let union : t -> t -> t =
   fun { references = lhs_references; type_var_loc_to_label_refs = lhs_tv }
       { references = rhs_references; type_var_loc_to_label_refs = rhs_tv } ->
    { references =
        Map.merge_skewed lhs_references rhs_references ~combine:(fun ~key:_loc ->
            Set.union)
    ; type_var_loc_to_label_refs =
        Map.merge_skewed lhs_tv rhs_tv ~combine:(fun ~key:_loc ->
            Map.merge_skewed ~combine:(fun ~key:_label -> Set.union))
    }


  (** Updates the references map, adding [usage_loc] to the list of locations mentioning [def]. *)
  let add_def_usage : def:def -> usage_loc:Location.t -> t -> t =
   fun ~def ~usage_loc ->
    map_ref
    @@ fun refs ->
    Map.change refs (Def.get_location def) ~f:(function
        | None -> Some (LSet.singleton usage_loc)
        | Some locs -> Some (Set.add locs usage_loc))


  let add_label_type_var_usage : Type_var.t -> Label.t -> env -> t -> t =
   fun tvar (Label (_, loc) as label) env refs ->
    Option.value
      ~default:refs
      (let open Option.Let_syntax in
      let%map orig_type = Env.lookup_tvar_opt env tvar in
      let t_loc = Def.get_location orig_type in
      map_tv
        (fun tvs ->
          Map.change tvs t_loc ~f:(fun lmap ->
              let lmap = Option.value ~default:Label.Map.empty lmap in
              return
              @@ Map.change
                   lmap
                   label
                   ~f:
                     (Option.some
                     <@ Option.value_map
                          ~default:(LSet.singleton loc)
                          ~f:(Fn.flip Set.add loc))))
        refs)


  (** Wrapper over [add_def_usage].
      If the def option is [Some def], calls [add_def_usage] with [def].
      Else, leave the reference map untouched *)
  let add_def_opt_usage : Location.t -> def option -> t -> t =
   fun usage_loc def_opt refs ->
    match def_opt with
    | Some def -> add_def_usage ~def ~usage_loc refs
    | None -> refs


  (** [add_vvar] looks up the [Value_var.t] in the [env] and updates
      the [references] with the help of [add_def_opt_usage] *)
  let add_vvar : Value_var.t -> env -> t -> t =
   fun v env ->
    let def_opt = Env.lookup_vvar_opt env v in
    add_def_opt_usage (Value_var.get_location v) def_opt


  (** [add_tvar] looks up the [Type_var.t] in the [env] and updates
      the [references] with the help of [add_def_opt_usage] *)
  let add_tvar : Type_var.t -> env -> t -> t =
   fun t env ->
    let def_opt = Env.lookup_tvar_opt env t in
    add_def_opt_usage (Type_var.get_location t) def_opt


  (** [add_ctor] looks up the [Ast_core.ty_expr] in the [env] and
      tries to pick the original label location inside sum/record type.
      If type is a variable then it places the necessary info into [type_var_loc_to_label_refs]. *)
  let add_ctor : Label.t -> env -> t -> t =
   fun (Label (_, loc) as label) env refs ->
    Option.value
      ~default:refs
      (let open Option.Let_syntax in
      let%bind label_type = Map.find env.label_types loc in
      let label_type = Ast_core.strip_abstraction label_type in
      match label_type.type_content with
      | T_sum (row, _) | T_record row ->
        let%map orig_label =
          Core.List.find ~f:(Label.equal label) (Map.keys row.fields)
        in
        add_def_usage ~def:(Label orig_label) ~usage_loc:loc refs
      | T_variable tvar when not (Type_var.is_generated tvar) ->
        return @@ add_label_type_var_usage tvar label env refs
      | T_module_accessor { module_path; element }
        when not (Type_var.is_generated element) ->
        let rec resolve_mpath module_path avail_defs =
          match module_path with
          | [] -> avail_defs
          | mv :: mvs ->
            Option.value
              ~default:avail_defs
              (let open Option.Let_syntax in
              let%bind mv = Env.lookup_mvar_in_defs_opt mv avail_defs in
              let%map _, defs = Module_map.resolve_mvar mv env.module_map in
              resolve_mpath mvs defs)
        in
        let avail_defs = resolve_mpath module_path env.avail_defs in
        let env = { env with avail_defs } in
        return @@ add_label_type_var_usage element label env refs
      | _ -> None)


  (* TODO : Shouldn't this be the same as the two above ?? *)

  (** [add_module_usages] takes [module_usages] and updates the [references]
      for each [Module_var.t] it adds a usage [Location.t] to it's [references] *)
  let add_module_usages : module_usages -> t -> t =
   fun module_usages references ->
    let add_module_usage : t -> module_usage -> t =
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
      :  update_element_reference:('a -> env -> t -> t) -> 'a -> Module_var.t
      -> module_map -> label_types -> t -> t
    =
   fun ~update_element_reference element mv module_map label_types ->
    match Module_map.resolve_mvar mv module_map with
    | None -> Fn.id
    | Some (_orig, defs) ->
      update_element_reference
        element
        { parent = Env.empty_defs
        ; avail_defs = defs
        ; module_map
        ; parent_mod = Some mv
        ; label_types
        }


  (** [add_maccess_vvar] updates references of a module accessed value/var *)
  let add_maccess_vvar
      : Value_var.t -> Module_var.t -> module_map -> label_types -> t -> t
    =
    add_maccess ~update_element_reference:add_vvar


  (** [add_maccess_tvar] updates references of a module accessed type *)
  let add_maccess_tvar : Type_var.t -> Module_var.t -> module_map -> label_types -> t -> t
    =
    add_maccess ~update_element_reference:add_tvar
end

type t = References.t
type references = t
type label_types = References.label_types

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
    :  Module_var.t Simple_utils.List.Ne.t -> env
    -> (Module_var.t * module_usages, module_usages) result
  =
 fun mvs env ->
  let init : module_usages = [] in
  let f : module_usages -> _ -> module_usages =
   fun acc (input, real, _resolved, _defs_of_that_module) ->
    let input_loc = Module_var.get_location input in
    (real, `Usage input_loc) :: acc
  in
  let defs = Env.union_defs env.avail_defs env.parent in
  let mmap = env.module_map in
  let acc, m_opt = Env.fold_resolve_mpath mvs defs mmap ~init ~f in
  match m_opt with
  | None -> Error acc
  | Some (_real, resolved, _defs) -> Ok (resolved, acc)


(** [resolve_module_alias_and_update_mvar_references] will resolve the module path
    with the help of [resolve_module_alias_in_env] and in addition to that it'll
    update the references of all the module path elements. *)
let resolve_module_alias_and_update_mvar_references
    :  ?update_references_for_element:
         ('a -> Module_var.t -> module_map -> label_types -> references -> references)
    -> ?element:'a -> Module_var.t Simple_utils.List.Ne.t -> env -> references
    -> references * defs_or_alias option
  =
 fun ?(update_references_for_element = fun _ _ _ _ rs -> rs) ?element mvs env refs ->
  let ma_res = resolve_mpath mvs env in
  match ma_res with
  | Ok (ma, module_usages) ->
    let refs = References.add_module_usages module_usages refs in
    let refs =
      Option.value_map element ~default:refs ~f:(fun element ->
          update_references_for_element element ma env.module_map env.label_types refs)
    in
    refs, Some (Alias ma)
  | Error module_usages ->
    let refs = References.add_module_usages module_usages refs in
    refs, None


(* --------------------------- AST traversal -------------------------------- *)

(** [expression] take an [AST.expression] & traverses its sub-expressions and
    upates the [references] & [env] *)
let rec expression : AST.expression -> references -> env -> references =
 fun e refs env ->
  match e.expression_content with
  | E_variable v -> References.add_vvar v env refs
  | E_contract mods ->
    let usages =
      mods
      |> Simple_utils.List.Ne.to_list
      |> List.map ~f:(fun x -> x, `Usage (Module_var.get_location x))
    in
    References.add_module_usages usages refs
  | E_module_accessor { module_path; element } ->
    let refs, _ =
      resolve_module_alias_and_update_mvar_references
        (Simple_utils.List.Ne.of_list module_path)
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
  | E_type_abstraction { type_binder; result } ->
    let env = Env.add_tvar type_binder env in
    expression result refs env
  | E_let_mut_in { let_binder; rhs; let_result; attributes = _ }
  | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
    let labels = Linear_pattern.labels let_binder in
    let refs =
      List.fold_left
        ~init:refs
        ~f:(fun acc label -> References.add_ctor label env acc)
        labels
    in
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
  | E_constructor { constructor; element } ->
    let refs = References.add_ctor constructor env refs in
    expression element refs env
  | E_matching { matchee; disc_label; cases } ->
    let refs =
      Option.value_map
        (let open Option.Let_syntax in
        let%bind disc_label = disc_label in
        let%map row =
          match matchee.expression_content with
          | E_ascription { type_annotation; _ } ->
            let%map row, _ = AST.get_t_sum type_annotation in
            row
          | _ -> None
        in
        disc_label, row)
        ~default:refs
        ~f:(fun (Label (disc_label, usage_loc), row) ->
          let labels = Record.labels row.fields in
          List.fold_left labels ~init:refs ~f:(fun refs (Label (_, loc)) ->
              References.add_def_usage
                ~def:(Label (Label.T.create ~loc disc_label))
                ~usage_loc
                refs))
    in
    let refs = expression matchee refs env in
    List.fold cases ~init:refs ~f:(fun refs { pattern; body } ->
        let labels = Linear_pattern.labels pattern in
        let refs =
          List.fold_left
            ~init:refs
            ~f:(fun acc label -> References.add_ctor label env acc)
            labels
        in
        let binders = Linear_pattern.binders pattern in
        let vars = List.map binders ~f:Binder.get_var in
        let env = List.fold_right vars ~init:env ~f:Env.add_vvar in
        expression body refs env)
  | E_record e_label_map ->
    let refs =
      let labels = Record.labels e_label_map in
      List.fold_left
        ~init:refs
        ~f:(fun acc label -> References.add_ctor label env acc)
        labels
    in
    Record.fold e_label_map ~init:refs ~f:(fun refs e -> expression e refs env)
  | E_tuple es ->
    let refs =
      let labels = Record.labels_of_tuple es in
      List.fold_left
        ~init:refs
        ~f:(fun acc label -> References.add_ctor label env acc)
        labels
    in
    Simple_utils.List.Ne.fold_left es ~init:refs ~f:(fun refs e -> expression e refs env)
  | E_array items | E_array_as_list items ->
    List.fold_left items ~init:refs ~f:(fun entry item ->
        let entry =
          match item with
          | Expr_entry entry -> entry
          | Rest_entry entry -> entry
        in
        expression entry refs env)
  | E_accessor { struct_; path } ->
    let refs = References.add_ctor path env refs in
    expression struct_ refs env
  | E_update { struct_; path; update } ->
    let refs = References.add_ctor path env refs in
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
    let refs, defs_or_alias_opt, module_map =
      module_expression (Some module_binder) rhs refs env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    expression let_result refs env


(** [type_expression] takes a [AST.type_expression] it uses the [env] to
    update the [references]  *)
and type_expression : AST.type_expression -> references -> env -> references =
 fun te refs env ->
  match te.type_content with
  | T_variable tv -> References.add_tvar tv env refs
  | T_contract_parameter mods ->
    let usages =
      mods
      |> Simple_utils.List.Ne.to_list
      |> List.map ~f:(fun x -> x, `Usage (Module_var.get_location x))
    in
    References.add_module_usages usages refs
  | T_constant _ -> refs (* FIXME *)
  | T_module_accessor { module_path; element } ->
    let refs, _ =
      resolve_module_alias_and_update_mvar_references
        (Simple_utils.List.Ne.of_list module_path)
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
        let refs, _ =
          resolve_module_alias_and_update_mvar_references
            (Simple_utils.List.Ne.of_list module_path)
            env
            refs
            ~element
            ~update_references_for_element:References.add_maccess_tvar
        in
        refs
    in
    List.fold arguments ~init:refs ~f:(fun refs te -> type_expression te refs env)
  | T_singleton _ -> refs
  | T_sum ({ layout = _; fields }, _) | T_record { layout = _; fields } ->
    Record.fold fields ~init:refs ~f:(fun refs te -> type_expression te refs env)
  | T_arrow { type1; type2; param_names = _ } ->
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
    :  Module_var.t option -> AST.module_expr -> references -> env
    -> references * defs_or_alias option * module_map
  =
 fun parent_mod me refs env ->
  (* Move [avail_defs] to [parent] before finding [references] in module_expr *)
  let env =
    { env with
      avail_defs = Env.empty_defs
    ; parent = Env.union_defs env.avail_defs env.parent
    ; parent_mod
    }
  in
  let refs, defs_or_alias_opt, env =
    match me.wrap_content with
    | M_struct decls ->
      let refs, env = declarations decls refs env in
      refs, Some (Defs env.avail_defs), env
    | M_variable mv ->
      let refs, alias_opt =
        resolve_module_alias_and_update_mvar_references (mv, []) env refs
      in
      refs, alias_opt, env
    | M_module_path mvs ->
      let refs, alias_opt =
        resolve_module_alias_and_update_mvar_references mvs env refs
      in
      refs, alias_opt, env
  in
  refs, defs_or_alias_opt, env.module_map


and signature_expression
    :  AST.signature_expr -> references -> env
    -> references * defs_or_alias option * module_map
  =
 fun me refs env ->
  (* TODO: do we really want to treat a signature as a module? Don't we want to assume
     they are coming from the module that implements them?
     Note: I guess we still need to add the module names as references, and for
     signatures, we need to add sig items as references. And for both, we need to link the
     definitions with the references. *)
  let env =
    { env with
      avail_defs = Env.empty_defs
    ; parent = Env.union_defs env.avail_defs env.parent
    }
  in
  let refs, alias_opt, env =
    match me.wrap_content with
    | S_sig sig' ->
      let refs, env = signature sig' refs env in
      refs, Some (Defs env.avail_defs), env
    | S_path mvs ->
      let refs, alias_opt =
        resolve_module_alias_and_update_mvar_references mvs env refs
      in
      refs, alias_opt, env
  in
  refs, alias_opt, env.module_map


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
    let labels = Linear_pattern.labels pattern in
    let refs =
      List.fold_left
        ~init:refs
        ~f:(fun acc label -> References.add_ctor label env acc)
        labels
    in
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
  | D_module { module_binder; module_; module_attr = _; annotation } ->
    let ((refs, defs_or_alias_opt, module_map) as result) =
      module_expression (Some module_binder) module_ refs env
    in
    let env = Env.add_mvar module_binder defs_or_alias_opt module_map env in
    let refs, _defs_or_alias_opt, _module_map =
      Option.value_map annotation ~default:result ~f:(fun annotation ->
          let refs, defs_or_alias_opt, module_map =
            signature_expression annotation.signature refs env
          in
          (* TODO: not sure what to do here? It seems to work, but it's not clear whether
             it's correct. *)
          References.union (Tuple3.get1 result) refs, defs_or_alias_opt, module_map)
    in
    refs, env
  | D_module_include module_expr ->
    let refs, defs_or_alias_opt, module_map =
      module_expression env.parent_mod module_expr refs env
    in
    let env = Env.include_mvar defs_or_alias_opt module_map env in
    refs, env
  | D_signature { signature_binder; signature; signature_attr = _ } ->
    let refs, defs_or_alias_opt, module_map = signature_expression signature refs env in
    let env = Env.add_mvar signature_binder defs_or_alias_opt module_map env in
    refs, env
  | D_import { import_name; imported_module; import_attr = _ } ->
    let module_expr =
      Location.wrap ~loc:Location.generated (Module_expr.M_variable imported_module)
    in
    let refs, defs_or_alias_opt, module_map =
      module_expression env.parent_mod module_expr refs env
    in
    let env = Env.add_mvar import_name defs_or_alias_opt module_map env in
    refs, env


and sig_item : AST.sig_item -> references -> env -> references * env =
 fun s refs env ->
  match Location.unwrap s with
  | S_value (var, ty_expr, _attr) ->
    let refs = type_expression ty_expr refs env in
    let env = Env.add_vvar var env in
    refs, env
  | S_type (var, ty_expr, _) ->
    let refs = type_expression ty_expr refs env in
    let env = Env.add_tvar var env in
    refs, env
  | S_type_var (var, _) ->
    let env = Env.add_tvar var env in
    refs, env
  | S_module (var, sig') | S_module_type (var, sig') ->
    let refs, module_map = signature sig' refs env in
    let env = Env.add_mvar var None env.module_map env in
    refs, env
  | S_include signature ->
    let refs, _defs_or_alias_opt, _module_map = signature_expression signature refs env in
    refs, env


(** [declarations] takes a list of [AST.declaration], [references] & [env]
    and for each declaration it calls the [declaration] function with updates
    the [references] & [env] *)
and declarations : AST.declaration list -> references -> env -> references * env =
 fun decls refs env ->
  List.fold decls ~init:(refs, env) ~f:(fun (refs, env) decl -> declaration decl refs env)


and signature : AST.signature -> references -> env -> references * env =
 fun sig' refs env ->
  List.fold sig'.items ~init:(refs, env) ~f:(fun (refs, env) item ->
      sig_item item refs env)


let declarations : AST.declaration list -> label_types -> references =
 fun decls label_types ->
  let refs : references =
    { references = LMap.empty; type_var_loc_to_label_refs = LMap.empty }
  in
  let env = { Env.empty with label_types } in
  let refs, _ = declarations decls refs env in
  refs


type patched_references = References.references

(** [patch_references] picks [type_var_loc_to_label_refs] and
    tries to get the type content from defs list. If the type is record or sum
    then references would be updated. *)
let rec patch_references : references -> Types.def list -> patched_references =
 fun refs defs ->
  let refs =
    List.fold_left
      ~init:refs
      ~f:
        (fun acc -> function
          | Type tdef ->
            Option.value
              ~default:acc
              (let open Option.Let_syntax in
              let%bind orig_type = tdef.content in
              let%bind label_refs = Map.find refs.type_var_loc_to_label_refs tdef.range in
              let%map orig_labels =
                match (Ast_core.strip_abstraction orig_type).type_content with
                | T_sum (row, _) | T_record row -> Some (Map.keys row.fields)
                | _ -> None
              in
              List.fold_left
                ~init:acc
                ~f:(fun acc (Label (_, orig_label_loc) as orig_label) ->
                  Option.value
                    ~default:acc
                    (let%map refs = Map.find label_refs orig_label in
                     References.map_ref
                       (Fn.flip
                          Map.change
                          orig_label_loc
                          ~f:(return <@ Option.value_map ~default:refs ~f:(Set.union refs)))
                       acc))
                orig_labels)
          | Module { mod_case = Def defs; _ } ->
            { acc with references = patch_references acc defs }
          | Variable _ | Module { mod_case = Alias _; _ } | Label _ -> acc)
      defs
  in
  refs.references


let patch : references -> Types.def list -> Types.def list =
 fun refs defs ->
  let refs = patch_references refs defs in
  let rec patch refs defs =
    List.map defs ~f:(function
        | Types.Variable v ->
          (match Map.find refs v.range with
          | None -> Types.Variable v
          | Some references -> Variable { v with references })
        | Type t ->
          (match Map.find refs t.range with
          | None -> Types.Type t
          | Some references -> Type { t with references })
        | Label l ->
          (match Map.find refs l.range with
          | None -> Types.Label l
          | Some references -> Label { l with references })
        | Module m ->
          let patch_mod_case = function
            | Types.Alias _ as alias -> alias
            | Def defs -> Def (patch refs defs)
          in
          let patch_implementation = function
            | Types.Ad_hoc_signature defs -> Types.Ad_hoc_signature (patch refs defs)
            | Standalone_signature_or_module _ as path -> path
          in
          let m =
            match Map.find refs m.range with
            | None -> m
            | Some references -> { m with references }
          in
          Module
            { m with
              mod_case = patch_mod_case m.mod_case
            ; implements = List.map ~f:patch_implementation m.implements
            })
  in
  patch refs defs
