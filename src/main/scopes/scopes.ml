open Ligo_prim
open Types
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module Formatter = Formatter
module Api_helper = Api_helper
module LSet = Types.LSet
module Location = Simple_utils.Location

type def = Types.def
type scopes = Types.scopes

type typing_env =
  { type_env : Environment.t
  ; bindings : Misc.bindings_map
  }

let rec drop_last : 'a list -> 'a * 'a list =
 fun xs ->
  match xs with
  | [] -> failwith "empty list"
  | [ x ] -> x, []
  | x :: xs ->
    let last, xs = drop_last xs in
    last, x :: xs


let update_typing_env
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.declaration -> typing_env
  =
 fun ~with_types ~options tenv decl ->
  match with_types with
  | true ->
    let typed_prg =
      Simple_utils.Trace.to_option
      @@ Checking.type_declaration ~options ~env:tenv.type_env decl
    in
    (match typed_prg with
    | Some decl ->
      let module AST = Ast_typed in
      let bindings =
        Misc.extract_variable_types tenv.bindings decl.wrap_content
      in
      let type_env = Environment.add_declaration decl tenv.type_env in
      { type_env; bindings }
    | None -> tenv)
  | false -> tenv


type reference =
  | Variable of VVar.t
  | Type of TVar.t
  | ModuleAccessType of MVar.t list * TVar.t
  | ModuleAccess of MVar.t list * VVar.t
  | ModuleAlias of MVar.t list

(* let pp_reference : Format.formatter -> reference -> unit
  = fun f r ->
      let () = Format.fprintf f "\n" in
      match r with
        Variable v -> Format.fprintf f "%a" VVar.pp v
      | ModuleAccess (mvs, v) ->
          let () = List.iter mvs ~f:(fun mv -> Format.fprintf f "%a." MVar.pp mv) in
          Format.fprintf f "%a" VVar.pp v
      | ModuleAlias mvs ->
          List.iter mvs ~f:(fun mv -> Format.fprintf f "%a." MVar.pp mv)
let pp_references : Format.formatter -> reference list -> unit
  = fun f refs ->
      List.iter refs ~f:(pp_reference f) *)

let get_location_of_module_path : MVar.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (MVar.get_location m))


let rec update_variable_reference : VVar.t -> def list -> bool * def list =
 fun ev defs ->
  match defs with
  | [] -> false, []
  | Variable v :: defs when VVar.is_name ev v.name ->
    let loc = VVar.get_location ev in
    let references = LSet.add loc v.references in
    true, Variable { v with references } :: defs
  | def :: defs ->
    let updated, defs = update_variable_reference ev defs in
    updated, def :: defs


let rec update_type_variable_reference : TVar.t -> def list -> bool * def list =
 fun tv defs ->
  match defs with
  | [] -> false, []
  | Type t :: defs when TVar.is_name tv t.name ->
    let loc = TVar.get_location tv in
    let references = LSet.add loc t.references in
    true, Type { t with references } :: defs
  | def :: defs ->
    let updated, defs = update_type_variable_reference tv defs in
    updated, def :: defs


let rec update_module_variable_references
    :  MVar.t list -> [ `Variable of VVar.t | `Type of TVar.t | `None ]
    -> def list -> bool * def list
  =
 fun mvs ev defs ->
  match mvs, defs with
  | _, [] -> false, defs
  | [], defs ->
    (match ev with
    | `Variable ev -> update_variable_reference ev defs
    | `Type tv -> update_type_variable_reference tv defs
    | `None -> true, defs)
  | mv :: mvs, Module ({ name; mod_case = Def d; _ } as m) :: defs
    when MVar.is_name mv name ->
    let loc = MVar.get_location mv in
    let references = LSet.add loc m.references in
    let updated, d = update_module_variable_references mvs ev d in
    let mod_case = Def d in
    updated, Module { m with mod_case; references } :: defs
  | mv :: mvs, Module ({ name; mod_case = Alias a; _ } as m) :: defs
    when MVar.is_name mv name ->
    let loc = MVar.get_location mv in
    let references = LSet.add loc m.references in
    let updated, defs = resolve_alias a mvs ev defs in
    updated, Module { m with references } :: defs
  | mvs, def :: defs ->
    let updated, defs = update_module_variable_references mvs ev defs in
    updated, def :: defs


and resolve_alias
    :  string list -> MVar.t list
    -> [ `Variable of VVar.t | `Type of TVar.t | `None ] -> def list
    -> bool * def list
  =
 fun aliases mvs ev defs ->
  match aliases with
  | [] -> update_module_variable_references mvs ev defs
  | alias :: aliases ->
    let rec aux = function
      | [] -> false, []
      | Module ({ name; mod_case = Def d; _ } as m) :: defs
        when String.(name = alias) ->
        let updated, d = resolve_alias aliases mvs ev d in
        let mod_case = Def d in
        updated, Module { m with mod_case } :: defs
      | (Module { name; mod_case = Alias a; _ } as def) :: defs
        when String.(name = alias) ->
        let updated, defs = resolve_alias (a @ aliases) mvs ev defs in
        updated, def :: defs
      | def :: defs ->
        let updated, defs = aux defs in
        updated, def :: defs
    in
    aux defs


let update_reference : reference -> def list -> bool * def list =
 fun r defs ->
  match r with
  | Variable ev -> update_variable_reference ev defs
  | Type tv -> update_type_variable_reference tv defs
  | ModuleAccessType (mvs, tv) ->
    update_module_variable_references mvs (`Type tv) defs
  | ModuleAccess (mvs, ev) ->
    update_module_variable_references mvs (`Variable ev) defs
  | ModuleAlias mvs -> update_module_variable_references mvs `None defs


let update_references : reference list -> def list -> def list * reference list =
 fun refs defs ->
  let defs, refs =
    List.fold_left refs ~init:(defs, []) ~f:(fun (defs, refs) r ->
        let updated, defs = update_reference r defs in
        let refs = if updated then refs else r :: refs in
        defs, refs)
  in
  defs, refs


let rec find_type_references : AST.type_expression -> reference list =
 fun te ->
  match te.type_content with
  | T_variable t ->
    let t = TVar.set_location te.location t in
    [ Type t ]
  | T_sum { fields; layout = _ } | T_record { fields; layout = _ } ->
    Record.fold fields ~init:[] ~f:(fun refs row ->
        let t_refs = find_type_references row.associated_type in
        refs @ t_refs)
  | T_arrow { type1; type2 } ->
    find_type_references type1 @ find_type_references type2
  | T_app { type_operator; arguments } ->
    let type_operator = TVar.set_location te.location type_operator in
    let t_refs = List.concat @@ List.map arguments ~f:find_type_references in
    Type type_operator :: t_refs
  | T_module_accessor { module_path; element } ->
    [ ModuleAccessType (module_path, element) ]
  | T_singleton _ -> []
  | T_abstraction { ty_binder; kind = _; type_ }
  | T_for_all { ty_binder; kind = _; type_ } ->
    let t_refs = find_type_references type_ in
    List.filter t_refs ~f:(fun r ->
        match r with
        | Type tv -> not @@ TVar.equal tv ty_binder
        | Variable _ | ModuleAccess _ | ModuleAlias _ | ModuleAccessType _ ->
          true)


let find_binder_type_references
    : AST.type_expression option Binder.t -> reference list
  =
 fun b ->
  let ascr = Binder.get_ascr b in
  match ascr with
  | Some t -> find_type_references t
  | None -> []


let find_param_type_references
    : AST.type_expression option Param.t -> reference list
  =
 fun p ->
  let ascr = Param.get_ascr p in
  match ascr with
  | Some t -> find_type_references t
  | None -> []


let rec expression
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.expression -> def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv e ->
  let expression = expression ~with_types ~options in
  match e.expression_content with
  | E_literal _ -> [], [], tenv, [ e.location, [] ]
  | E_raw_code _ -> [], [], tenv, [ e.location, [] ]
  | E_variable ev -> [], [ Variable ev ], tenv, [ e.location, [] ]
  | E_module_accessor m ->
    [], [ ModuleAccess (m.module_path, m.element) ], tenv, [ e.location, [] ]
  | E_constant { arguments; _ } ->
    let defs, refs, tenv, scopes =
      List.fold_left
        arguments
        ~init:([], [], tenv, [])
        ~f:(fun (defs, refs, tenv, scopes) e ->
          let ds, rs, tenv, scopes' = expression tenv e in
          ds @ defs, rs @ refs, tenv, scopes @ scopes')
    in
    defs, refs, tenv, merge_same_scopes scopes
  | E_application { lamb; args } ->
    let defs, refs, tenv, scopes = expression tenv lamb in
    let scopes = merge_same_scopes scopes in
    let defs', refs', tenv, scopes' = expression tenv args in
    let scopes' = merge_same_scopes scopes' in
    let scopes_final = merge_same_scopes (scopes @ scopes') in
    defs' @ defs, refs' @ refs, tenv, scopes_final
  | E_lambda { binder; result; output_type = _ } ->
    let t_refs = find_param_type_references binder in
    let var = Param.get_var binder in
    let core_type = Param.get_ascr binder in
    let def =
      if VVar.is_generated var
      then []
      else (
        let binder_loc = VVar.get_location var in
        [ Misc.make_v_def
            ~with_types
            ?core_type
            tenv.bindings
            Local
            var
            binder_loc
            result.location
        ])
    in
    let defs_result, refs_result, tenv, scopes = expression tenv result in
    let scopes = merge_same_scopes scopes in
    let defs, refs = update_references (refs_result @ t_refs) def in
    defs_result @ defs, refs, tenv, add_defs_to_scopes def scopes
  | E_type_abstraction { result; _ } -> expression tenv result
  | E_constructor { element; _ } -> expression tenv element
  | E_accessor { struct_; _ } -> expression tenv struct_
  | E_ascription { anno_expr; type_annotation } ->
    let t_refs = find_type_references type_annotation in
    let defs, refs, env, scopes = expression tenv anno_expr in
    defs, refs @ t_refs, env, scopes
  | E_update { struct_; update; _ } ->
    let defs, refs, tenv, scopes = expression tenv struct_ in
    let defs', refs', tenv, scopes' = expression tenv update in
    defs' @ defs, refs' @ refs, tenv, merge_same_scopes scopes @ scopes'
  | E_while { cond; body } ->
    let defs, refs, tenv, scopes = expression tenv cond in
    let defs', refs', tenv, scopes' = expression tenv body in
    defs' @ defs, refs' @ refs, tenv, merge_same_scopes scopes @ scopes'
  | E_for { binder; start; incr; final; f_body } ->
    let def =
      if VVar.is_generated binder
      then []
      else (
        let binder_loc = VVar.get_location binder in
        [ Misc.make_v_def
            ~with_types
            tenv.bindings
            Local
            binder
            binder_loc
            start.location
        ])
    in
    let defs_start, refs_start, tenv, scopes1 = expression tenv start in
    let defs_incr, refs_incr, tenv, scopes2 = expression tenv incr in
    let defs_final, refs_final, tenv, scopes3 = expression tenv final in
    let defs_body, refs_body, tenv, scopes4 = expression tenv f_body in
    let scopes4 = add_defs_to_scopes def scopes4 in
    let scopes =
      merge_same_scopes scopes1
      @ merge_same_scopes scopes2
      @ merge_same_scopes scopes3
      @ scopes4
    in
    let defs, refs_body = update_references refs_body def in
    ( defs_start @ defs_incr @ defs_final @ defs_body @ defs
    , refs_start @ refs_incr @ refs_final @ refs_body
    , tenv
    , add_defs_to_scopes def scopes )
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    let binders = binder1 :: Option.to_list binder2 in
    let defs =
      binders
      |> List.filter ~f:VVar.is_generated
      |> List.map ~f:(fun binder ->
             let loc = VVar.get_location binder in
             Misc.make_v_def
               ~with_types
               tenv.bindings
               Local
               binder
               loc
               collection.location)
    in
    let defs_coll, refs_coll, tenv, scopes_coll = expression tenv collection in
    let defs_body, refs_body, tenv, scopes_body = expression tenv fe_body in
    let scopes_body = add_defs_to_scopes defs scopes_body in
    let scopes = merge_same_scopes scopes_coll @ scopes_body in
    let defs, refs_body = update_references refs_body defs in
    defs_body @ defs_coll @ defs, refs_body @ refs_coll, tenv, scopes
  | E_record e_lable_map ->
    let defs, refs, tenv, scopes =
      Record.LMap.fold
        (fun _ e (defs, refs, tenv, scopes) ->
          let defs', refs', tenv, scopes' = expression tenv e in
          let scopes' = merge_same_scopes scopes' in
          defs' @ defs, refs' @ refs, tenv, merge_same_scopes scopes @ scopes')
        e_lable_map
        ([], [], tenv, [])
    in
    defs, refs, tenv, scopes
  | E_assign { binder; expression = e } ->
    let t_refs = find_binder_type_references binder in
    let refs' = [ Variable (Binder.get_var binder) ] in
    let defs, refs, tenv, scopes = expression tenv e in
    let scopes = merge_same_scopes scopes in
    defs, refs @ refs' @ t_refs, tenv, scopes
  | E_let_in
      { let_binder
      ; rhs = { expression_content = E_recursive _; _ } as rhs
      ; let_result
      ; _
      } ->
    let t_refs = find_binder_type_references let_binder in
    (* For recursive functions we don't need to add a def for [let_binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_rhs] *)
    let defs_rhs, refs_rhs, tenv, scopes = expression tenv rhs in
    let defs_rhs, refs_rhs = update_references refs_rhs defs_rhs in
    let def, defs_rhs = drop_last defs_rhs in
    let defs_result, refs_result, tenv, scopes' = expression tenv let_result in
    let scopes' = add_defs_to_scopes [ def ] scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    let defs, refs = update_references (refs_rhs @ refs_result @ t_refs) [ def ] in
    defs_result @ defs_rhs @ defs, refs, tenv, scopes
  | E_let_mut_in { let_binder; rhs; let_result; _ }
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let t_refs = find_binder_type_references let_binder in
    let var = Binder.get_var let_binder in
    let core_type = Binder.get_ascr let_binder in
    let defs_binder =
      if VVar.is_generated var
      then []
      else (
        let binder_loc = VVar.get_location var in
        [ Misc.make_v_def
            ~with_types
            ?core_type
            tenv.bindings
            Local
            var
            binder_loc
            rhs.location
        ])
    in
    let defs_rhs, refs_rhs, tenv, scopes = expression tenv rhs in
    let defs_result, refs_result, tenv, scopes' = expression tenv let_result in
    let scopes' = add_defs_to_scopes defs_binder scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    let defs, refs = update_references (refs_rhs @ refs_result @ t_refs) defs_binder in
    defs_result @ defs_rhs @ defs, refs, tenv, scopes
  | E_recursive { fun_name; fun_type; lambda = { binder; result; _ } } ->
    let t_refs = find_type_references fun_type in
    let t_refs = t_refs @ find_type_references (Param.get_ascr binder) in
    let def_fun =
      let binder_loc = VVar.get_location fun_name in
      Misc.make_v_def
        ~with_types
        ~core_type:fun_type
        tenv.bindings
        Local
        fun_name
        binder_loc
        result.location
    in
    let def_par =
      let var = Param.get_var binder in
      let core_type = Param.get_ascr binder in
      if VVar.is_generated var
      then []
      else (
        let binder_loc = VVar.get_location var in
        [ Misc.make_v_def
            ~with_types
            ~core_type
            tenv.bindings
            Local
            var
            binder_loc
            result.location
        ])
    in
    let defs_result, refs_result, tenv, scopes = expression tenv result in
    let scopes = merge_same_scopes scopes in
    let defs = def_par @ [ def_fun ] in
    let defs, refs = update_references (refs_result @ t_refs) defs in
    ( defs_result @ defs
    , refs
    , tenv
    , add_defs_to_scopes (def_par @ [ def_fun ]) scopes )
  | E_type_in { type_binder; rhs; let_result } ->
    let t_refs = find_type_references rhs in
    let def = type_expression type_binder Local rhs in
    let defs, refs, tenv, scopes = expression tenv let_result in
    let scopes = merge_same_scopes scopes in
    [ def ] @ defs, refs @ t_refs, tenv, scopes
  | E_matching { matchee; cases } ->
    let defs_matchee, refs_matchee, tenv, scopes = expression tenv matchee in
    let defs_matchee, refs_matchee =
      update_references refs_matchee defs_matchee
    in
    let scopes = merge_same_scopes scopes in
    let defs_cases, refs_cases, tenv, scopes' =
      List.fold_left
        cases
        ~init:([], [], tenv, [])
        ~f:(fun (defs, refs, tenv, scopes) { pattern; body } ->
          let defs_pat, refs_pat =
            AST.Pattern.fold_pattern
              (fun (defs, refs) (p : _ AST.Pattern.t) ->
                match p.wrap_content with
                | P_var binder ->
                  let t_refs = find_binder_type_references binder in
                  let def =
                    let var = Binder.get_var binder in
                    let core_type = Binder.get_ascr binder in
                    let binder_loc = VVar.get_location var in
                    Misc.make_v_def
                      ~with_types
                      ?core_type
                      tenv.bindings
                      Local
                      var
                      binder_loc
                      body.location
                  in
                  def :: defs, refs @ t_refs
                | P_unit
                | P_list (Cons _)
                | P_list (List _)
                | P_variant _ | P_tuple _ | P_record _ -> defs, refs)
              ([], [])
              pattern
          in
          let defs_body, refs_body, tenv, scopes' = expression tenv body in
          let scopes' = merge_same_scopes scopes' in
          let scopes' = add_defs_to_scopes defs_pat scopes' in
          let defs_pat, refs_body = update_references refs_body defs_pat in
          ( defs_body @ defs_pat @ defs
          , refs_body @ refs @ refs_pat
          , tenv
          , merge_same_scopes scopes @ scopes' ))
    in
    defs_matchee @ defs_cases, refs_matchee @ refs_cases, tenv, scopes @ scopes'
  | E_mod_in { module_binder; rhs; let_result } ->
    let defs_module, refs_module, tenv, scopes =
      module_expression ~with_types ~options tenv Local module_binder rhs
    in
    let defs_result, refs_result, tenv, scopes' = expression tenv let_result in
    let scopes' = merge_same_scopes scopes' in
    let scopes' = add_defs_to_scopes defs_module scopes' in
    let defs_module, refs_result = update_references refs_result defs_module in
    let defs, refs_result =
      update_references refs_result (defs_result @ defs_module)
    in
    defs, refs_result @ refs_module, tenv, merge_same_scopes scopes @ scopes'


and type_expression : TVar.t -> def_type -> AST.type_expression -> def =
 fun tv def_type t ->
  let def =
    let binder_name = get_type_binder_name tv in
    let binder_loc = TVar.get_location tv in
    make_t_def binder_name def_type binder_loc t
  in
  def


and module_expression
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> def_type -> MVar.t -> AST.module_expr
    -> def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv def_type top m ->
  match m.wrap_content with
  | M_struct decls ->
    let defs, refs, tenv, scopes =
      declarations ~with_types ~options tenv decls
    in
    let range = MVar.get_location top in
    let body_range = m.location in
    let name = get_mod_binder_name top in
    let def = make_m_def ~range ~body_range name def_type defs in
    [ def ], refs, tenv, scopes
  | M_variable mv ->
    let def, reference =
      let name = get_mod_binder_name top in
      let range = MVar.get_location top in
      let body_range = MVar.get_location mv in
      let alias = [ get_mod_binder_name mv ] in
      let def = make_m_alias_def ~range ~body_range name def_type alias in
      let reference = ModuleAlias [ mv ] in
      def, reference
    in
    [ def ], [ reference ], tenv, []
  | M_module_path path ->
    let mvs = List.Ne.to_list path in
    let def, reference =
      let name = get_mod_binder_name top in
      let range = MVar.get_location top in
      let body_range = get_location_of_module_path mvs in
      let alias = List.map mvs ~f:get_mod_binder_name in
      let def = make_m_alias_def ~range ~body_range name def_type alias in
      let reference = ModuleAlias mvs in
      def, reference
    in
    [ def ], [ reference ], tenv, []


and declaration_expression
    :  with_types:bool -> options:Compiler_options.middle_end
    -> ?core_type:AST.type_expression -> typing_env -> VVar.t -> AST.expression
    -> def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options ?core_type tenv ev e ->
  let defs, refs, tenv, scopes = expression ~with_types ~options tenv e in
  let range = VVar.get_location ev in
  let body_range = e.location in
  let def =
    Misc.make_v_def
      ~with_types
      ?core_type
      tenv.bindings
      Global
      ev
      range
      body_range
  in
  [ def ] @ defs, refs, tenv, scopes


and declaration
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.declaration -> def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv decl ->
  let tenv = update_typing_env ~with_types ~options tenv decl in
  match decl.wrap_content with
  | D_value { attr = { hidden; _ }; _ }
  | D_module { module_attr = { hidden; _ }; _ }
  | D_type { type_attr = { hidden; _ }; _ }
    when hidden -> [], [], tenv, []
  | D_value
      { binder
      ; expr = { expression_content = E_recursive _; _ } as expr
      ; attr = _
      } ->
    let t_refs = find_binder_type_references binder in
    (* For recursive functions we don't need to add a def for [binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_expr] *)
    let defs_expr, refs_rhs, tenv, scopes =
      expression ~with_types ~options tenv expr
    in
    let def, defs_expr = drop_last defs_expr in
    defs_expr @ [ def ], refs_rhs @ t_refs, tenv, scopes
  | D_value { binder; expr; attr = _ } ->
    let t_refs = find_binder_type_references binder in
    let var = Binder.get_var binder in
    let core_type = Binder.get_ascr binder in
    let defs, refs, env, scopes =
      declaration_expression ~with_types ~options ?core_type tenv var expr
    in
    defs, refs @ t_refs, env, scopes
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let t_refs = find_type_references type_expr in
    let def = type_expression type_binder Global type_expr in
    [ def ], t_refs, tenv, []
  | D_module { module_binder; module_; module_attr = _ } ->
    let defs, refs, env, scopes =
      module_expression ~with_types ~options tenv Global module_binder module_
    in
    let defs, refs = update_references refs defs in
    defs, refs, env, scopes


and declarations
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.declaration list -> def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv decls ->
  let defs, refs, tenv, scopes =
    List.fold_left
      decls
      ~init:([], [], tenv, [])
      ~f:(fun (defs, refs, tenv, scopes) decl ->
        let defs', refs', tenv, scopes' =
          declaration ~with_types ~options tenv decl
        in
        let scopes' = add_defs_to_scopes (filter_local_defs defs) scopes' in
        let defs, refs = update_references (refs' @ refs) defs in
        defs' @ defs, refs, tenv, scopes @ scopes')
  in
  let defs, refs = update_references refs defs in
  defs, refs, tenv, scopes


let resolve_module_aliases_to_module_ids : def list -> def list =
 fun defs ->
  let find_mod_def_id ids name =
    match List.find ids ~f:(fun (name', _) -> String.(name = name')) with
    | Some (_, id) -> id
    | None -> name
  in
  let rec resolve defs mod_def_ids =
    match defs with
    | [] -> mod_def_ids, defs
    | Module ({ uid; name; mod_case = Def d; _ } as m) :: defs ->
      let d = List.rev d in
      let mod_def_ids, d = resolve d mod_def_ids in
      let d = List.rev d in
      let mod_case = Def d in
      let mod_def_ids, defs = resolve defs ((name, uid) :: mod_def_ids) in
      mod_def_ids, Module { m with mod_case } :: defs
    | Module ({ uid; name; mod_case = Alias a; _ } as m) :: defs ->
      let a = List.map a ~f:(find_mod_def_id mod_def_ids) in
      let mod_case = Alias a in
      let mod_def_ids, defs = resolve defs ((name, uid) :: mod_def_ids) in
      mod_def_ids, Module { m with mod_case } :: defs
    | def :: defs ->
      let mod_def_ids, defs = resolve defs mod_def_ids in
      mod_def_ids, def :: defs
  in
  let defs = List.rev defs in
  let defs = snd (resolve defs []) in
  List.rev defs


let scopes
    :  with_types:bool -> options:Compiler_options.middle_end -> AST.module_
    -> def list * scopes
  =
 fun ~with_types ~options prg ->
  let () = reset_counter () in
  let tenv =
    { type_env = options.init_env; bindings = Misc.Bindings_map.empty }
  in
  let defs, _, _, scopes = declarations ~with_types ~options tenv prg in
  let scopes = fix_shadowing_in_scopes scopes in
  let defs = resolve_module_aliases_to_module_ids defs in
  defs, scopes

(* TODO: update references for types *)