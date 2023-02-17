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
module Trace = Simple_utils.Trace
module Types = Types

[@@@landmark "auto"]

(* To profile
   Build: dune build --instrument-with landarks ./src/bin/runligo.exe
   Run:   OCAML_LANDMARKS=auto _build/default/src/bin/runligo.exe info get-scope x.mligo --format dev --with-types
*)

module PP = PP

type def = Types.def
type scopes = Types.scopes

type typing_env =
  { type_env : Environment.t
  ; bindings : Misc.bindings_map
  ; decls : Ast_typed.declaration list
  }

let rec drop_last : 'a list -> 'a * 'a list =
 fun xs ->
  match xs with
  | [] -> failwith "empty list"
  | [ x ] -> x, []
  | x :: xs ->
    let last, xs = drop_last xs in
    last, x :: xs


let set_core_type_if_possible
    :  AST.type_expression option Binder.t list -> AST.expression
    -> AST.type_expression option Binder.t list * AST.expression
  =
 fun binders expr ->
  match binders, expr.expression_content with
  | [ binder ], AST.E_ascription { anno_expr; type_annotation } ->
    let binder = Binder.set_ascr binder (Some type_annotation) in
    [ binder ], anno_expr
  | _ -> binders, expr


let collect_warns_and_errs
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    tracer
    (e, ws)
  =
  let () = List.iter ws ~f:raise.warning in
  raise.log_error (tracer e)


let checking
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~options
    tenv
    decl
  =
  let typed_prg =
    Simple_utils.Trace.to_stdlib_result
    @@ Checking.type_declaration ~options ~env:tenv.type_env decl
  in
  Result.(
    match typed_prg with
    | Ok (decl, ws) ->
      let module AST = Ast_typed in
      let bindings = Misc.extract_variable_types tenv.bindings decl.wrap_content in
      let type_env = Environment.add_declaration decl tenv.type_env in
      let decls = tenv.decls @ [ decl ] in
      let () = List.iter ws ~f:raise.warning in
      { type_env; bindings; decls }
    | Error (e, ws) ->
      collect_warns_and_errs ~raise Main_errors.checking_tracer (e, ws);
      tenv)


let checking_self_pass
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~(options : Compiler_options.middle_end)
    tenv
  =
  match
    let warn_unused_rec = options.warn_unused_rec in
    Simple_utils.Trace.to_stdlib_result
    @@ Self_ast_typed.all_program ~warn_unused_rec tenv.decls
  with
  | Ok (_, ws) -> List.iter ws ~f:raise.warning
  | Error (e, ws) ->
    collect_warns_and_errs ~raise Main_errors.self_ast_typed_tracer (e, ws)


let update_typing_env
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise -> with_types:bool
    -> options:Compiler_options.middle_end -> typing_env -> AST.declaration -> typing_env
  =
 fun ~raise ~with_types ~options tenv decl ->
  match with_types with
  | true ->
    let tenv = checking ~raise ~options tenv decl in
    let () = checking_self_pass ~raise ~options tenv in
    tenv
  | false -> tenv


type reference =
  | Variable of VVar.t
  | Type of TVar.t
  | ModuleAccess of MVar.t list * [ `Variable of VVar.t | `Type of TVar.t ]
  | ModuleAlias of MVar.t list

let pp_reference : Format.formatter -> reference -> unit =
 fun f r ->
  let () = Format.fprintf f "\n" in
  match r with
  | Variable v -> Format.fprintf f "%a" VVar.pp v
  | Type t -> Format.fprintf f "%a" TVar.pp t
  | ModuleAccess (mvs, `Variable v) ->
    let () = List.iter mvs ~f:(fun mv -> Format.fprintf f "%a." MVar.pp mv) in
    Format.fprintf f "%a" VVar.pp v
  | ModuleAccess (mvs, `Type t) ->
    let () = List.iter mvs ~f:(fun mv -> Format.fprintf f "%a." MVar.pp mv) in
    Format.fprintf f "%a" TVar.pp t
  | ModuleAlias mvs -> List.iter mvs ~f:(fun mv -> Format.fprintf f "%a." MVar.pp mv)


let[@warning "-32"] pp_references : Format.formatter -> reference list -> unit =
 fun f refs -> List.iter refs ~f:(pp_reference f)


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
    :  MVar.t list -> [ `Variable of VVar.t | `Type of TVar.t | `None ] -> def list
    -> bool * def list
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
    :  string list -> MVar.t list -> [ `Variable of VVar.t | `Type of TVar.t | `None ]
    -> def list -> bool * def list
  =
 fun aliases mvs ev defs ->
  match aliases with
  | [] -> update_module_variable_references mvs ev defs
  | alias :: aliases ->
    let rec aux = function
      | [] -> false, []
      | Module ({ name; mod_case = Def d; _ } as m) :: defs when String.(name = alias) ->
        let updated, d = resolve_alias aliases mvs ev d in
        let mod_case = Def d in
        updated, Module { m with mod_case } :: defs
      | (Module { name; mod_case = Alias a; _ } as def) :: defs when String.(name = alias)
        ->
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
  | ModuleAccess (mvs, `Type tv) -> update_module_variable_references mvs (`Type tv) defs
  | ModuleAccess (mvs, `Variable ev) ->
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


let join_defs_and_top_defs
    : def list -> [ `Toplevel of def list ] -> (def list * string) option
  =
 fun defs (`Toplevel top_defs) ->
  let rec join defs top_defs =
    match defs with
    | [] -> None
    | [ def ] -> Some (def :: top_defs, get_def_uid def)
    | def :: defs ->
      Option.map ~f:(fun (defs, uid) -> def :: defs, uid) (join defs top_defs)
  in
  join defs top_defs


let split_defs : def list -> string -> def list * def list =
 fun defs uid ->
  let rec split defs =
    match defs with
    | [] -> [], []
    | def :: defs ->
      if String.equal (get_def_uid def) uid
      then [ def ], defs
      else (
        let local, top = split defs in
        def :: local, top)
  in
  split defs


let update_references
    :  reference list -> def list -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * reference list
  =
 fun refs defs (`Toplevel top_defs) ->
  match join_defs_and_top_defs defs (`Toplevel top_defs) with
  | Some (defs, uid) ->
    let defs, refs = update_references refs defs in
    let defs, top_defs = split_defs defs uid in
    `Toplevel top_defs, defs, refs
  | None ->
    let top_defs, refs = update_references refs top_defs in
    `Toplevel top_defs, defs, refs


let rec find_type_references : AST.type_expression -> reference list =
 fun te ->
  match te.type_content with
  | T_variable t ->
    let t = TVar.set_location te.location t in
    [ Type t ]
  | T_sum { fields; layout = _ } | T_record { fields; layout = _ } ->
    Record.fold fields ~init:[] ~f:(fun refs row ->
        let t_refs = find_type_references row in
        refs @ t_refs)
  | T_arrow { type1; type2 } -> find_type_references type1 @ find_type_references type2
  | T_app { type_operator; arguments } ->
    (* TODO: unignore the path on `type_operator`, update `ModuleAccessType`? *)
    let type_operator =
      TVar.set_location te.location (Module_access.get_el @@ type_operator)
    in
    let t_refs = List.concat @@ List.map arguments ~f:find_type_references in
    Type type_operator :: t_refs
  | T_module_accessor { module_path; element } ->
    [ ModuleAccess (module_path, `Type element) ]
  | T_singleton _ -> []
  | T_abstraction { ty_binder; kind = _; type_ }
  | T_for_all { ty_binder; kind = _; type_ } ->
    let t_refs = find_type_references type_ in
    List.filter t_refs ~f:(fun r ->
        match r with
        | Type tv -> not @@ TVar.equal tv ty_binder
        | Variable _ | ModuleAccess _ | ModuleAlias _ -> true)


let find_binder_type_references : AST.type_expression option Binder.t -> reference list =
 fun b ->
  let ascr = Binder.get_ascr b in
  match ascr with
  | Some t -> find_type_references t
  | None -> []


let find_pattern_type_references
    : AST.type_expression option AST.Pattern.t -> reference list
  =
 fun pattern ->
  pattern |> AST.Pattern.binders |> List.concat_map ~f:find_binder_type_references


let find_param_type_references : AST.type_expression option Param.t -> reference list =
 fun p ->
  let ascr = Param.get_ascr p in
  match ascr with
  | Some t -> find_type_references t
  | None -> []


let find_top_level_definion : [ `Toplevel of def list ] -> string -> def option =
 fun (`Toplevel top_defs) uid ->
  let rec find defs =
    match defs with
    | [] -> None
    | def :: defs ->
      (match def with
      | Module mdef when String.equal mdef.uid uid -> Some (Module mdef)
      | Variable vdef when String.equal vdef.uid uid -> Some (Variable vdef)
      | Type tdef when String.equal tdef.uid uid -> Some (Type tdef)
      | Variable _ | Type _ -> find defs
      | Module { mod_case = Alias _; _ } -> find defs
      | Module { mod_case = Def inner; _ } ->
        (match find inner with
        | Some d -> Some d
        | None -> find defs))
  in
  find top_defs


let patch_top_level_references : def list -> [ `Toplevel of def list ] -> def list =
 fun defs (`Toplevel top_defs) ->
  let add_references def =
    match find_top_level_definion (`Toplevel top_defs) (get_def_uid def) with
    | Some d -> Some (add_references_to_def def (get_references d))
    | None -> None
  in
  let rec patch (defs : def list) =
    match defs with
    | [] -> []
    | (Module ({ mod_case = Def inner; _ } as mdef) as def) :: defs ->
      let def =
        match add_references def with
        | Some def -> def
        | None -> def
      in
      let inner = patch inner in
      Module { mdef with mod_case = Def inner; references = get_references def }
      :: patch defs
    | ((Variable _ | Type _ | Module { mod_case = Alias _; _ }) as def) :: defs ->
      (match add_references def with
      | Some def -> def :: patch defs
      | None -> def :: patch defs)
  in
  patch defs


let rec expression ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.expression -> [ `Toplevel of def list ] -> [ `Local of def list ]
    -> [ `Local of def list ]
       * [ `Toplevel of def list ]
       * def list
       * reference list
       * typing_env
       * scopes
  =
 fun ~with_types ~options tenv e (`Toplevel top_defs) (`Local local_defs) ->
  let expression = expression ~raise ~with_types ~options in
  match e.expression_content with
  | E_literal _ -> `Local local_defs, `Toplevel top_defs, [], [], tenv, [ e.location, [] ]
  | E_raw_code _ ->
    `Local local_defs, `Toplevel top_defs, [], [], tenv, [ e.location, [] ]
  | E_variable ev ->
    `Local local_defs, `Toplevel top_defs, [], [ Variable ev ], tenv, [ e.location, [] ]
  | E_module_accessor m ->
    ( `Local local_defs
    , `Toplevel top_defs
    , []
    , [ ModuleAccess (m.module_path, `Variable m.element) ]
    , tenv
    , [ e.location, [] ] )
  | E_constant { arguments; _ } ->
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      List.fold_left
        arguments
        ~init:(`Local local_defs, `Toplevel top_defs, [], [], tenv, [])
        ~f:(fun (`Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes) e ->
          let `Local local_defs, `Toplevel top_defs, ds, rs, tenv, scopes' =
            expression tenv e (`Toplevel top_defs) (`Local local_defs)
          in
          ( `Local (ds @ local_defs)
          , `Toplevel top_defs
          , ds @ defs
          , rs @ refs
          , tenv
          , scopes @ scopes' ))
    in
    `Local local_defs, `Toplevel top_defs, defs, refs, tenv, merge_same_scopes scopes
  | E_application { lamb; args } ->
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      expression tenv lamb (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes = merge_same_scopes scopes in
    let `Local local_defs, `Toplevel top_defs, defs', refs', tenv, scopes' =
      expression tenv args (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes' = merge_same_scopes scopes' in
    let scopes_final = merge_same_scopes (scopes @ scopes') in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , refs' @ refs
    , tenv
    , scopes_final )
  | E_lambda { binder; result; output_type } ->
    let binder_type_refs = find_param_type_references binder in
    let output_type_refs =
      Option.value ~default:[] @@ Option.map ~f:find_type_references output_type
    in
    let t_refs = binder_type_refs @ output_type_refs in
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
    let `Local local_defs, `Toplevel top_defs, defs_result, refs_result, tenv, scopes =
      expression tenv result (`Toplevel top_defs) (`Local (def @ local_defs))
    in
    let scopes = merge_same_scopes scopes in
    let `Toplevel top_defs, defs, refs =
      update_references (refs_result @ t_refs) def (`Toplevel top_defs)
    in
    ( `Local (defs_result @ local_defs)
    , `Toplevel top_defs
    , defs_result @ defs
    , refs
    , tenv
    , add_defs_to_scopes def scopes )
  | E_type_abstraction { result; _ } ->
    expression tenv result (`Toplevel top_defs) (`Local local_defs)
  | E_constructor { element; _ } ->
    expression tenv element (`Toplevel top_defs) (`Local local_defs)
  | E_accessor { struct_; _ } ->
    expression tenv struct_ (`Toplevel top_defs) (`Local local_defs)
  | E_ascription { anno_expr; type_annotation } ->
    let t_refs = find_type_references type_annotation in
    let `Local local_defs, `Toplevel top_defs, defs, refs, env, scopes =
      expression tenv anno_expr (`Toplevel top_defs) (`Local local_defs)
    in
    `Local local_defs, `Toplevel top_defs, defs, refs @ t_refs, env, scopes
  | E_update { struct_; update; _ } ->
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      expression tenv struct_ (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs', refs', tenv, scopes' =
      expression tenv update (`Toplevel top_defs) (`Local local_defs)
    in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , refs' @ refs
    , tenv
    , merge_same_scopes scopes @ scopes' )
  | E_while { cond; body } ->
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      expression tenv cond (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs', refs', tenv, scopes' =
      expression tenv body (`Toplevel top_defs) (`Local local_defs)
    in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , refs' @ refs
    , tenv
    , merge_same_scopes scopes @ scopes' )
  | E_for { binder; start; incr; final; f_body } ->
    let def =
      if VVar.is_generated binder
      then []
      else (
        let binder_loc = VVar.get_location binder in
        [ Misc.make_v_def ~with_types tenv.bindings Local binder binder_loc start.location
        ])
    in
    let `Local local_defs, `Toplevel top_defs, defs_start, refs_start, tenv, scopes1 =
      expression tenv start (`Toplevel top_defs) (`Local (def @ local_defs))
    in
    let `Local local_defs, `Toplevel top_defs, defs_incr, refs_incr, tenv, scopes2 =
      expression tenv incr (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_final, refs_final, tenv, scopes3 =
      expression tenv final (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_body, refs_body, tenv, scopes4 =
      expression tenv f_body (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes4 = add_defs_to_scopes def scopes4 in
    let scopes =
      merge_same_scopes scopes1
      @ merge_same_scopes scopes2
      @ merge_same_scopes scopes3
      @ scopes4
    in
    let `Toplevel top_defs, defs, refs_body =
      update_references refs_body def (`Toplevel top_defs)
    in
    ( `Local (defs_start @ defs_incr @ defs_final @ defs_body @ local_defs)
    , `Toplevel top_defs
    , defs_start @ defs_incr @ defs_final @ defs_body @ defs
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
    let `Local local_defs, `Toplevel top_defs, defs_coll, refs_coll, tenv, scopes_coll =
      expression tenv collection (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_body, refs_body, tenv, scopes_body =
      expression tenv fe_body (`Toplevel top_defs) (`Local (defs @ local_defs))
    in
    let scopes_body = add_defs_to_scopes defs scopes_body in
    let scopes = merge_same_scopes scopes_coll @ scopes_body in
    let `Toplevel top_defs, defs, refs_body =
      update_references refs_body defs (`Toplevel top_defs)
    in
    ( `Local (defs_body @ defs_coll @ local_defs)
    , `Toplevel top_defs
    , defs_body @ defs_coll @ defs
    , refs_body @ refs_coll
    , tenv
    , scopes )
  | E_record e_lable_map ->
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      Record.fold
        e_lable_map
        ~init:(`Local local_defs, `Toplevel top_defs, [], [], tenv, [])
        ~f:(fun (`Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes) e ->
          let `Local local_defs, `Toplevel top_defs, defs', refs', tenv, scopes' =
            expression tenv e (`Toplevel top_defs) (`Local local_defs)
          in
          let scopes' = merge_same_scopes scopes' in
          ( `Local (defs' @ local_defs)
          , `Toplevel top_defs
          , defs' @ defs
          , refs' @ refs
          , tenv
          , merge_same_scopes scopes @ scopes' ))
    in
    `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes
  | E_assign { binder; expression = e } ->
    let t_refs = find_binder_type_references binder in
    let refs' = [ Variable (Binder.get_var binder) ] in
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      expression tenv e (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes = merge_same_scopes scopes in
    `Local local_defs, `Toplevel top_defs, defs, refs @ refs' @ t_refs, tenv, scopes
  | E_let_in
      { let_binder
      ; rhs = { expression_content = E_recursive _; _ } as rhs
      ; let_result
      ; _
      } ->
    let t_refs = find_pattern_type_references let_binder in
    (* For recursive functions we don't need to add a def for [let_binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_rhs] *)
    let `Local local_defs, `Toplevel top_defs, defs_rhs, refs_rhs, tenv, scopes =
      expression tenv rhs (`Toplevel top_defs) (`Local local_defs)
    in
    let def, defs_rhs = drop_last defs_rhs in
    let `Local local_defs, `Toplevel top_defs, defs_result, refs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local ([ def ] @ local_defs))
    in
    let scopes' = add_defs_to_scopes [ def ] scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    let `Toplevel top_defs, defs, refs_result =
      update_references (refs_result @ t_refs) [ def ] (`Toplevel top_defs)
    in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_result @ defs_rhs @ defs
    , refs_result @ refs_rhs
    , tenv
    , scopes )
  | E_let_mut_in { let_binder; rhs; let_result; _ }
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let binders = AST.Pattern.binders let_binder in
    let binders, rhs = set_core_type_if_possible binders rhs in
    let t_refs = List.concat (List.map ~f:find_binder_type_references binders) in
    let defs_binder =
      List.concat_map binders ~f:(fun binder ->
          let var = Binder.get_var binder in
          let core_type = Binder.get_ascr binder in
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
            ]))
    in
    let `Local local_defs, `Toplevel top_defs, defs_rhs, refs_rhs, tenv, scopes =
      expression tenv rhs (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_result, refs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local (defs_binder @ local_defs))
    in
    let scopes' = add_defs_to_scopes defs_binder scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    let `Toplevel top_defs, defs, refs_result =
      update_references (refs_result @ t_refs) defs_binder (`Toplevel top_defs)
    in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_result @ defs_rhs @ defs
    , refs_result @ refs_rhs
    , tenv
    , scopes )
  | E_recursive
      { fun_name; fun_type; lambda = { binder; result; _ }; force_lambdarec = _ } ->
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
    let `Local local_defs, `Toplevel top_defs, defs_result, refs_result, tenv, scopes =
      expression tenv result (`Toplevel top_defs) (`Local (def_par @ local_defs))
    in
    let scopes = merge_same_scopes scopes in
    let defs = def_par @ [ def_fun ] in
    let `Toplevel top_defs, defs, refs =
      update_references (refs_result @ t_refs) defs (`Toplevel top_defs)
    in
    ( `Local (defs_result @ local_defs)
    , `Toplevel top_defs
    , defs_result @ defs
    , refs
    , tenv
    , add_defs_to_scopes (def_par @ [ def_fun ]) scopes )
  | E_type_in { type_binder; rhs; let_result } ->
    let t_refs = find_type_references rhs in
    let tdef = [ type_expression type_binder Local rhs ] in
    let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
      expression tenv let_result (`Toplevel top_defs) (`Local local_defs)
    in
    let `Toplevel top_defs, tdef, refs =
      update_references refs tdef (`Toplevel top_defs)
    in
    ( `Local (tdef @ local_defs)
    , `Toplevel top_defs
    , tdef @ defs
    , refs @ t_refs
    , tenv
    , add_defs_to_scopes tdef scopes )
  | E_matching { matchee; cases } ->
    let `Local local_defs, `Toplevel top_defs, defs_matchee, refs_matchee, tenv, scopes =
      expression tenv matchee (`Toplevel top_defs) (`Local local_defs)
    in
    let `Toplevel top_defs, defs_matchee, refs_matchee =
      update_references refs_matchee defs_matchee (`Toplevel top_defs)
    in
    let scopes = merge_same_scopes scopes in
    let `Local local_defs, `Toplevel top_defs, defs_cases, refs_cases, tenv, scopes' =
      List.fold_left
        cases
        ~init:(`Local local_defs, `Toplevel top_defs, [], [], tenv, [])
        ~f:
          (fun (`Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes)
               { pattern; body } ->
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
          let `Local local_defs, `Toplevel top_defs, defs_body, refs_body, tenv, scopes' =
            expression tenv body (`Toplevel top_defs) (`Local local_defs)
          in
          let scopes' = merge_same_scopes scopes' in
          let scopes' = add_defs_to_scopes defs_pat scopes' in
          let `Toplevel top_defs, defs_pat, refs_body =
            update_references refs_body defs_pat (`Toplevel top_defs)
          in
          ( `Local (defs_body @ defs_pat @ local_defs)
          , `Toplevel top_defs
          , defs_body @ defs_pat @ defs
          , refs_body @ refs @ refs_pat
          , tenv
          , merge_same_scopes scopes @ scopes' ))
    in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_matchee @ defs_cases
    , refs_matchee @ refs_cases
    , tenv
    , scopes @ scopes' )
  | E_mod_in { module_binder; rhs; let_result } ->
    let `Local local_defs, `Toplevel top_defs, defs_module, refs_module, tenv, scopes =
      match join_defs_and_top_defs local_defs (`Toplevel top_defs) with
      | Some (top_defs, uid) ->
        let `Toplevel top_defs, defs_module, refs_module, tenv, scopes =
          module_expression
            ~raise
            ~with_types
            ~options
            tenv
            Local
            module_binder
            rhs
            (`Toplevel top_defs)
        in
        let local_defs, top_defs = split_defs top_defs uid in
        `Local local_defs, `Toplevel top_defs, defs_module, refs_module, tenv, scopes
      | None ->
        let `Toplevel top_defs, defs_module, refs_module, tenv, scopes =
          module_expression
            ~raise
            ~with_types
            ~options
            tenv
            Local
            module_binder
            rhs
            (`Toplevel top_defs)
        in
        `Local local_defs, `Toplevel top_defs, defs_module, refs_module, tenv, scopes
    in
    (* let () =
      Format.printf
        "$$$$$$$$$$$$$$$$$$\n %a \n\n %a\n$$$$$$$$$$$$$$$$$$\n"
        PP.definitions local_defs
        pp_references
        refs_module
    in *)
    let `Local local_defs, `Toplevel top_defs, defs_result, refs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local (defs_module @ local_defs))
    in
    (* let () =
      Format.printf
        "##################\n %a \n\n %a\n##################\n"
        PP.definitions local_defs
        pp_references
        refs_result
    in *)
    let `Toplevel top_defs, local_defs, refs_result =
      update_references refs_result local_defs (`Toplevel top_defs)
    in
    let scopes' = merge_same_scopes scopes' in
    let scopes' = add_defs_to_scopes defs_module scopes' in
    let `Toplevel top_defs, defs, refs_result =
      update_references refs_result (defs_result @ defs_module) (`Toplevel top_defs)
    in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs
    , refs_result @ refs_module
    , tenv
    , merge_same_scopes scopes @ scopes' )


and type_expression : TVar.t -> def_type -> AST.type_expression -> def =
 fun tv def_type t ->
  let def =
    let binder_name = get_type_binder_name tv in
    let binder_loc = TVar.get_location tv in
    make_t_def binder_name def_type binder_loc t
  in
  def


and module_expression ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env -> def_type
    -> MVar.t -> AST.module_expr -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv def_type top m (`Toplevel top_defs) ->
  match m.wrap_content with
  | M_struct decls ->
    let `Toplevel top_defs, defs, refs, tenv, scopes =
      (* [update_tenv] is [false] because [update_typing_env] already types
         nested modules, so we only need to call it at toplevel declarations. *)
      declarations
        ~raise
        ~update_tenv:false
        ~with_types
        ~options
        tenv
        decls
        (`Toplevel top_defs)
    in
    let range = MVar.get_location top in
    let body_range = m.location in
    let name = get_mod_binder_name top in
    let def = make_m_def ~range ~body_range name def_type defs in
    `Toplevel top_defs, [ def ], refs, tenv, scopes
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
    `Toplevel top_defs, [ def ], [ reference ], tenv, []
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
    `Toplevel top_defs, [ def ], [ reference ], tenv, []


and declaration_expression ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.type_expression option Binder.t list -> AST.expression
    -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options tenv binders e (`Toplevel top_defs) ->
  let `Local local_defs, `Toplevel top_defs, defs, refs, tenv, scopes =
    expression ~raise ~with_types ~options tenv e (`Toplevel top_defs) (`Local [])
  in
  let defs = patch_top_level_references defs (`Toplevel local_defs) in
  let defs' =
    List.map binders ~f:(fun binder ->
        let core_type = Binder.get_ascr binder in
        let ev = Binder.get_var binder in
        let range = VVar.get_location ev in
        let body_range = e.location in
        Misc.make_v_def ~with_types ?core_type tenv.bindings Global ev range body_range)
  in
  `Toplevel top_defs, defs' @ defs, refs, tenv, scopes


and declaration ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> update_tenv:bool
    -> typing_env -> AST.declaration -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * reference list * typing_env * scopes
  =
 fun ~with_types ~options ~update_tenv tenv decl (`Toplevel top_defs) ->
  let tenv =
    if update_tenv
    then (* only at top-level *)
      update_typing_env ~raise ~with_types ~options tenv decl
    else tenv
  in
  match decl.wrap_content with
  | D_value { binder; expr = { expression_content = E_recursive _; _ } as expr; attr = _ }
    ->
    let t_refs = find_binder_type_references binder in
    (* For recursive functions we don't need to add a def for [binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_expr] *)
    let `Local local_defs, `Toplevel top_defs, defs_expr, refs_rhs, tenv, scopes =
      expression ~raise ~with_types ~options tenv expr (`Toplevel top_defs) (`Local [])
    in
    let defs_expr = patch_top_level_references defs_expr (`Toplevel local_defs) in
    let defs_expr = patch_top_level_references defs_expr (`Toplevel local_defs) in
    let def, defs_expr = drop_last defs_expr in
    `Toplevel top_defs, [ def ] @ defs_expr, refs_rhs @ t_refs, tenv, scopes
  | D_irrefutable_match { pattern; expr; _ } ->
    let binders = AST.Pattern.binders pattern in
    let binders, expr = set_core_type_if_possible binders expr in
    let t_refs = List.concat (List.map ~f:find_binder_type_references binders) in
    let `Toplevel top_defs, defs, refs, env, scopes =
      declaration_expression
        ~raise
        ~with_types
        ~options
        tenv
        binders
        expr
        (`Toplevel top_defs)
    in
    `Toplevel top_defs, defs, refs @ t_refs, env, scopes
  | D_value { binder; expr; attr = _ } ->
    let t_refs = find_binder_type_references binder in
    let `Toplevel top_defs, defs, refs, env, scopes =
      declaration_expression
        ~raise
        ~with_types
        ~options
        tenv
        [ binder ]
        expr
        (`Toplevel top_defs)
    in
    `Toplevel top_defs, defs, refs @ t_refs, env, scopes
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let t_refs = find_type_references type_expr in
    let def = type_expression type_binder Global type_expr in
    `Toplevel top_defs, [ def ], t_refs, tenv, []
  | D_module { module_binder; module_; module_attr = _ } ->
    let `Toplevel top_defs, defs, refs, env, scopes =
      module_expression
        ~raise
        ~with_types
        ~options
        tenv
        Global
        module_binder
        module_
        (`Toplevel top_defs)
    in
    let `Toplevel top_defs, defs, refs =
      update_references refs defs (`Toplevel top_defs)
    in
    `Toplevel top_defs, defs, refs, env, scopes


and declarations ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> ?update_tenv:bool
    -> ?stdlib_defs:def list -> ?top_level:bool -> typing_env -> AST.declaration list
    -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * reference list * typing_env * scopes
  =
 fun ~with_types
     ~options
     ?(update_tenv = true)
     ?(stdlib_defs = [])
     ?(top_level = false)
     tenv
     decls
     (`Toplevel top_defs) ->
  let `Toplevel top_defs, `Global gdefs, `Local ldefs, refs, tenv, scopes =
    List.fold_left
      decls
      ~init:(`Toplevel top_defs, `Global stdlib_defs, `Local [], [], tenv, [])
      ~f:
        (fun (`Toplevel top_defs, `Global gdefs, `Local ldefs, refs, tenv, scopes) decl ->
        let `Toplevel top_defs, defs', refs', tenv, scopes' =
          declaration
            ~raise
            ~with_types
            ~options
            ~update_tenv
            tenv
            decl
            (`Toplevel top_defs)
        in
        let `Global gdefs', `Local ldefs' = filter_local_defs defs' in
        let scopes' = add_defs_to_scopes gdefs scopes' in
        let `Toplevel top_defs, gdefs, refs =
          update_references (refs' @ refs) gdefs (`Toplevel top_defs)
        in
        let top_defs = if top_level then gdefs' @ top_defs else top_defs in
        ( `Toplevel top_defs
        , `Global (gdefs' @ gdefs)
        , `Local (ldefs' @ ldefs)
        , refs
        , tenv
        , scopes @ scopes' ))
  in
  let `Toplevel top_defs, gdefs, refs =
    update_references refs gdefs (`Toplevel top_defs)
  in
  `Toplevel top_defs, ldefs @ gdefs, refs, tenv, scopes


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


let stdlib_defs ~raise
    : options:Compiler_options.middle_end -> Ast_core.program -> def list
  =
 fun ~options stdlib_core ->
  let no_stdlib = options.no_stdlib in
  if no_stdlib
  then []
  else (
    let tenv =
      { type_env = options.init_env; bindings = Misc.Bindings_map.empty; decls = [] }
    in
    let `Toplevel top_defs, stdlib_defs, _, _, _ =
      declarations ~raise ~with_types:false ~options tenv stdlib_core (`Toplevel [])
    in
    let stdlib_defs = patch_top_level_references stdlib_defs (`Toplevel top_defs) in
    ignore_local_defs stdlib_defs)


let scopes
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise -> with_types:bool
    -> options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> AST.module_ -> def list * scopes
  =
 fun ~raise ~with_types ~options ~stdlib prg ->
  let () = reset_counter () in
  let stdlib, stdlib_core = stdlib in
  let type_env = Environment.append options.init_env stdlib in
  let tenv = { type_env; bindings = Misc.Bindings_map.empty; decls = [] } in
  let defs, scopes =
    let stdlib_defs = stdlib_defs ~raise ~options stdlib_core in
    let `Toplevel top_defs, defs, _, _, scopes =
      declarations
        ~raise
        ~with_types
        ~options
        ~stdlib_defs
        ~top_level:true
        tenv
        prg
        (`Toplevel stdlib_defs)
    in
    let defs = patch_top_level_references defs (`Toplevel top_defs) in
    defs, scopes
  in
  let scopes = fix_shadowing_in_scopes scopes in
  let defs = resolve_module_aliases_to_module_ids defs in
  defs, scopes
