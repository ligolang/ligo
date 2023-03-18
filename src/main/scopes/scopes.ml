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
    -> [ `Local of def list ] * [ `Toplevel of def list ] * def list * typing_env * scopes
  =
 fun ~with_types ~options tenv e (`Toplevel top_defs) (`Local local_defs) ->
  let expression = expression ~raise ~with_types ~options in
  match e.expression_content with
  | E_literal _ -> `Local local_defs, `Toplevel top_defs, [], tenv, [ e.location, [] ]
  | E_raw_code _ -> `Local local_defs, `Toplevel top_defs, [], tenv, [ e.location, [] ]
  | E_variable _ev -> `Local local_defs, `Toplevel top_defs, [], tenv, [ e.location, [] ]
  | E_module_accessor _m ->
    `Local local_defs, `Toplevel top_defs, [], tenv, [ e.location, [] ]
  | E_constant { arguments; _ } ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      List.fold_left
        arguments
        ~init:(`Local local_defs, `Toplevel top_defs, [], tenv, [])
        ~f:(fun (`Local local_defs, `Toplevel top_defs, defs, tenv, scopes) e ->
          let `Local local_defs, `Toplevel top_defs, ds, tenv, scopes' =
            expression tenv e (`Toplevel top_defs) (`Local local_defs)
          in
          `Local (ds @ local_defs), `Toplevel top_defs, ds @ defs, tenv, scopes @ scopes')
    in
    `Local local_defs, `Toplevel top_defs, defs, tenv, merge_same_scopes scopes
  | E_application { lamb; args } ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      expression tenv lamb (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes = merge_same_scopes scopes in
    let `Local local_defs, `Toplevel top_defs, defs', tenv, scopes' =
      expression tenv args (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes' = merge_same_scopes scopes' in
    let scopes_final = merge_same_scopes (scopes @ scopes') in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , tenv
    , scopes_final )
  | E_lambda { binder; result; output_type = _ } ->
    let var = Param.get_var binder in
    let core_type = Param.get_ascr binder in
    let def =
      let binder_loc = VVar.get_location var in
      Misc.make_v_def
        ~with_types
        ?core_type
        tenv.bindings
        Local
        var
        binder_loc
        result.location
      |> Option.to_list
    in
    let `Local local_defs, `Toplevel top_defs, defs_result, tenv, scopes =
      expression tenv result (`Toplevel top_defs) (`Local (def @ local_defs))
    in
    let scopes = merge_same_scopes scopes in
    ( `Local (defs_result @ local_defs)
    , `Toplevel top_defs
    , defs_result @ def
    , tenv
    , add_defs_to_scopes def scopes )
  | E_type_abstraction { result; _ } ->
    expression tenv result (`Toplevel top_defs) (`Local local_defs)
  | E_constructor { element; _ } ->
    expression tenv element (`Toplevel top_defs) (`Local local_defs)
  | E_accessor { struct_; _ } ->
    expression tenv struct_ (`Toplevel top_defs) (`Local local_defs)
  | E_ascription { anno_expr; type_annotation = _ } ->
    let `Local local_defs, `Toplevel top_defs, defs, env, scopes =
      expression tenv anno_expr (`Toplevel top_defs) (`Local local_defs)
    in
    `Local local_defs, `Toplevel top_defs, defs, env, scopes
  | E_update { struct_; update; _ } ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      expression tenv struct_ (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs', tenv, scopes' =
      expression tenv update (`Toplevel top_defs) (`Local local_defs)
    in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , tenv
    , merge_same_scopes scopes @ scopes' )
  | E_while { cond; body } ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      expression tenv cond (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs', tenv, scopes' =
      expression tenv body (`Toplevel top_defs) (`Local local_defs)
    in
    ( `Local (defs' @ defs @ local_defs)
    , `Toplevel top_defs
    , defs' @ defs
    , tenv
    , merge_same_scopes scopes @ scopes' )
  | E_for { binder; start; incr; final; f_body } ->
    let def =
      let binder_loc = VVar.get_location binder in
      Misc.make_v_def ~with_types tenv.bindings Local binder binder_loc start.location
      |> Option.to_list
    in
    let `Local local_defs, `Toplevel top_defs, defs_start, tenv, scopes1 =
      expression tenv start (`Toplevel top_defs) (`Local (def @ local_defs))
    in
    let `Local local_defs, `Toplevel top_defs, defs_incr, tenv, scopes2 =
      expression tenv incr (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_final, tenv, scopes3 =
      expression tenv final (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_body, tenv, scopes4 =
      expression tenv f_body (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes4 = add_defs_to_scopes def scopes4 in
    let scopes =
      merge_same_scopes scopes1
      @ merge_same_scopes scopes2
      @ merge_same_scopes scopes3
      @ scopes4
    in
    ( `Local (defs_start @ defs_incr @ defs_final @ defs_body @ local_defs)
    , `Toplevel top_defs
    , defs_start @ defs_incr @ defs_final @ defs_body @ def
    , tenv
    , add_defs_to_scopes def scopes )
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    let binders = binder1 :: Option.to_list binder2 in
    let defs =
      binders
      |> List.filter ~f:VVar.is_generated
      |> List.filter_map ~f:(fun binder ->
             let loc = VVar.get_location binder in
             Misc.make_v_def
               ~with_types
               tenv.bindings
               Local
               binder
               loc
               collection.location)
    in
    let `Local local_defs, `Toplevel top_defs, defs_coll, tenv, scopes_coll =
      expression tenv collection (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_body, tenv, scopes_body =
      expression tenv fe_body (`Toplevel top_defs) (`Local (defs @ local_defs))
    in
    let scopes_body = add_defs_to_scopes defs scopes_body in
    let scopes = merge_same_scopes scopes_coll @ scopes_body in
    ( `Local (defs_body @ defs_coll @ local_defs)
    , `Toplevel top_defs
    , defs_body @ defs_coll @ defs
    , tenv
    , scopes )
  | E_record e_lable_map ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      Record.fold
        e_lable_map
        ~init:(`Local local_defs, `Toplevel top_defs, [], tenv, [])
        ~f:(fun (`Local local_defs, `Toplevel top_defs, defs, tenv, scopes) e ->
          let `Local local_defs, `Toplevel top_defs, defs', tenv, scopes' =
            expression tenv e (`Toplevel top_defs) (`Local local_defs)
          in
          let scopes' = merge_same_scopes scopes' in
          ( `Local (defs' @ local_defs)
          , `Toplevel top_defs
          , defs' @ defs
          , tenv
          , merge_same_scopes scopes @ scopes' ))
    in
    `Local local_defs, `Toplevel top_defs, defs, tenv, scopes
  | E_assign { binder = _; expression = e } ->
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      expression tenv e (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes = merge_same_scopes scopes in
    `Local local_defs, `Toplevel top_defs, defs, tenv, scopes
  | E_let_in
      { let_binder = _
      ; rhs = { expression_content = E_recursive _; _ } as rhs
      ; let_result
      ; _
      } ->
    (* For recursive functions we don't need to add a def for [let_binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_rhs] *)
    let `Local local_defs, `Toplevel top_defs, defs_rhs, tenv, scopes =
      expression tenv rhs (`Toplevel top_defs) (`Local local_defs)
    in
    let def, defs_rhs = drop_last defs_rhs in
    let `Local local_defs, `Toplevel top_defs, defs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local ([ def ] @ local_defs))
    in
    let scopes' = add_defs_to_scopes [ def ] scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    `Local local_defs, `Toplevel top_defs, defs_result @ defs_rhs @ [ def ], tenv, scopes
  | E_let_mut_in { let_binder; rhs; let_result; _ }
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let binders = AST.Pattern.binders let_binder in
    let binders, rhs = set_core_type_if_possible binders rhs in
    let defs_binder =
      List.filter_map binders ~f:(fun binder ->
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
            rhs.location)
    in
    let `Local local_defs, `Toplevel top_defs, defs_rhs, tenv, scopes =
      expression tenv rhs (`Toplevel top_defs) (`Local local_defs)
    in
    let `Local local_defs, `Toplevel top_defs, defs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local (defs_binder @ local_defs))
    in
    let scopes' = add_defs_to_scopes defs_binder scopes' in
    let scopes = merge_same_scopes scopes @ scopes' in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_result @ defs_rhs @ defs_binder
    , tenv
    , scopes )
  | E_recursive
      { fun_name; fun_type; lambda = { binder; result; _ }; force_lambdarec = _ } ->
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
      |> Option.to_list
    in
    let def_par =
      let var = Param.get_var binder in
      let core_type = Param.get_ascr binder in
      let binder_loc = VVar.get_location var in
      Misc.make_v_def
        ~with_types
        ~core_type
        tenv.bindings
        Local
        var
        binder_loc
        result.location
      |> Option.to_list
    in
    let `Local local_defs, `Toplevel top_defs, defs_result, tenv, scopes =
      expression tenv result (`Toplevel top_defs) (`Local (def_par @ local_defs))
    in
    let scopes = merge_same_scopes scopes in
    let defs = def_par @ def_fun in
    ( `Local (defs_result @ local_defs)
    , `Toplevel top_defs
    , defs_result @ defs
    , tenv
    , add_defs_to_scopes (def_par @ def_fun) scopes )
  | E_type_in { type_binder; rhs; let_result } ->
    let tdef = [ type_expression type_binder Local rhs ] in
    let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
      expression tenv let_result (`Toplevel top_defs) (`Local local_defs)
    in
    ( `Local (tdef @ local_defs)
    , `Toplevel top_defs
    , tdef @ defs
    , tenv
    , add_defs_to_scopes tdef scopes )
  | E_matching { matchee; cases } ->
    let `Local local_defs, `Toplevel top_defs, defs_matchee, tenv, scopes =
      expression tenv matchee (`Toplevel top_defs) (`Local local_defs)
    in
    let scopes = merge_same_scopes scopes in
    let `Local local_defs, `Toplevel top_defs, defs_cases, tenv, scopes' =
      List.fold_left
        cases
        ~init:(`Local local_defs, `Toplevel top_defs, [], tenv, [])
        ~f:
          (fun (`Local local_defs, `Toplevel top_defs, defs, tenv, scopes)
               { pattern; body } ->
          let defs_pat =
            AST.Pattern.fold_pattern
              (fun defs (p : _ AST.Pattern.t) ->
                match p.wrap_content with
                | P_var binder ->
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
                    |> Option.to_list
                  in
                  def @ defs
                | P_unit
                | P_list (Cons _)
                | P_list (List _)
                | P_variant _ | P_tuple _ | P_record _ -> defs)
              []
              pattern
          in
          let `Local local_defs, `Toplevel top_defs, defs_body, tenv, scopes' =
            expression tenv body (`Toplevel top_defs) (`Local local_defs)
          in
          let scopes' = merge_same_scopes scopes' in
          let scopes' = add_defs_to_scopes defs_pat scopes' in
          ( `Local (defs_body @ defs_pat @ local_defs)
          , `Toplevel top_defs
          , defs_body @ defs_pat @ defs
          , tenv
          , merge_same_scopes scopes @ scopes' ))
    in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_matchee @ defs_cases
    , tenv
    , scopes @ scopes' )
  | E_mod_in { module_binder; rhs; let_result } ->
    let `Local local_defs, `Toplevel top_defs, defs_module, tenv, scopes =
      match join_defs_and_top_defs local_defs (`Toplevel top_defs) with
      | Some (top_defs, uid) ->
        let `Toplevel top_defs, defs_module, tenv, scopes =
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
        `Local local_defs, `Toplevel top_defs, defs_module, tenv, scopes
      | None ->
        let `Toplevel top_defs, defs_module, tenv, scopes =
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
        `Local local_defs, `Toplevel top_defs, defs_module, tenv, scopes
    in
    let `Local local_defs, `Toplevel top_defs, defs_result, tenv, scopes' =
      expression tenv let_result (`Toplevel top_defs) (`Local (defs_module @ local_defs))
    in
    let scopes' = merge_same_scopes scopes' in
    let scopes' = add_defs_to_scopes defs_module scopes' in
    ( `Local local_defs
    , `Toplevel top_defs
    , defs_result @ defs_module
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
    -> [ `Toplevel of def list ] * def list * typing_env * scopes
  =
 fun ~with_types ~options tenv def_type top m (`Toplevel top_defs) ->
  match m.wrap_content with
  | M_struct decls ->
    let `Toplevel top_defs, defs, tenv, scopes =
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
    `Toplevel top_defs, [ def ], tenv, scopes
  | M_variable mv ->
    let def =
      let name = get_mod_binder_name top in
      let range = MVar.get_location top in
      let body_range = MVar.get_location mv in
      let alias = [ get_mod_binder_name mv ] in
      let def = make_m_alias_def ~range ~body_range name def_type alias in
      def
    in
    `Toplevel top_defs, [ def ], tenv, []
  | M_module_path path ->
    let mvs = List.Ne.to_list path in
    let def =
      let name = get_mod_binder_name top in
      let range = MVar.get_location top in
      let body_range = Misc.get_location_of_module_path mvs in
      let alias = List.map mvs ~f:get_mod_binder_name in
      let def = make_m_alias_def ~range ~body_range name def_type alias in
      def
    in
    `Toplevel top_defs, [ def ], tenv, []


and declaration_expression ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> typing_env
    -> AST.type_expression option Binder.t list -> AST.expression
    -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * typing_env * scopes
  =
 fun ~with_types ~options tenv binders e (`Toplevel top_defs) ->
  let `Local local_defs, `Toplevel top_defs, defs, tenv, scopes =
    expression ~raise ~with_types ~options tenv e (`Toplevel top_defs) (`Local [])
  in
  let defs = patch_top_level_references defs (`Toplevel local_defs) in
  let defs' =
    List.filter_map binders ~f:(fun binder ->
        let core_type = Binder.get_ascr binder in
        let ev = Binder.get_var binder in
        let range = VVar.get_location ev in
        let body_range = e.location in
        Misc.make_v_def ~with_types ?core_type tenv.bindings Global ev range body_range)
  in
  `Toplevel top_defs, defs' @ defs, tenv, scopes


and declaration ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> update_tenv:bool
    -> typing_env -> AST.declaration -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * typing_env * scopes
  =
 fun ~with_types ~options ~update_tenv tenv decl (`Toplevel top_defs) ->
  let tenv =
    if update_tenv
    then (* only at top-level *)
      update_typing_env ~raise ~with_types ~options tenv decl
    else tenv
  in
  match decl.wrap_content with
  | D_value
      { binder = _; expr = { expression_content = E_recursive _; _ } as expr; attr = _ }
    ->
    (* For recursive functions we don't need to add a def for [binder]
        becase it will be added by the [E_recursive] case we just need to extract it
        out of the [defs_expr] *)
    let `Local local_defs, `Toplevel top_defs, defs_expr, tenv, scopes =
      expression ~raise ~with_types ~options tenv expr (`Toplevel top_defs) (`Local [])
    in
    let defs_expr = patch_top_level_references defs_expr (`Toplevel local_defs) in
    let defs_expr = patch_top_level_references defs_expr (`Toplevel local_defs) in
    let def, defs_expr = drop_last defs_expr in
    `Toplevel top_defs, [ def ] @ defs_expr, tenv, scopes
  | D_irrefutable_match { pattern; expr; _ } ->
    let binders = AST.Pattern.binders pattern in
    let binders, expr = set_core_type_if_possible binders expr in
    let `Toplevel top_defs, defs, env, scopes =
      declaration_expression
        ~raise
        ~with_types
        ~options
        tenv
        binders
        expr
        (`Toplevel top_defs)
    in
    `Toplevel top_defs, defs, env, scopes
  | D_value { binder; expr; attr = _ } ->
    let `Toplevel top_defs, defs, env, scopes =
      declaration_expression
        ~raise
        ~with_types
        ~options
        tenv
        [ binder ]
        expr
        (`Toplevel top_defs)
    in
    `Toplevel top_defs, defs, env, scopes
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let def = type_expression type_binder Global type_expr in
    `Toplevel top_defs, [ def ], tenv, []
  | D_module { module_binder; module_; module_attr = _ } ->
    let `Toplevel top_defs, defs, env, scopes =
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
    `Toplevel top_defs, defs, env, scopes


and declarations ~raise
    :  with_types:bool -> options:Compiler_options.middle_end -> ?update_tenv:bool
    -> ?stdlib_defs:def list -> ?top_level:bool -> typing_env -> AST.declaration list
    -> [ `Toplevel of def list ]
    -> [ `Toplevel of def list ] * def list * typing_env * scopes
  =
 fun ~with_types
     ~options
     ?(update_tenv = true)
     ?(stdlib_defs = [])
     ?(top_level = false)
     tenv
     decls
     (`Toplevel top_defs) ->
  let `Toplevel top_defs, `Global gdefs, `Local ldefs, tenv, scopes =
    List.fold_left
      decls
      ~init:(`Toplevel top_defs, `Global stdlib_defs, `Local [], tenv, [])
      ~f:(fun (`Toplevel top_defs, `Global gdefs, `Local ldefs, tenv, scopes) decl ->
        let `Toplevel top_defs, defs', tenv, scopes' =
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
        let top_defs = if top_level then gdefs' @ top_defs else top_defs in
        ( `Toplevel top_defs
        , `Global (gdefs' @ gdefs)
        , `Local (ldefs' @ ldefs)
        , tenv
        , scopes @ scopes' ))
  in
  `Toplevel top_defs, ldefs @ gdefs, tenv, scopes


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
    let `Toplevel top_defs, stdlib_defs, _, _ =
      declarations ~raise ~with_types:false ~options tenv stdlib_core (`Toplevel [])
    in
    let stdlib_defs = patch_top_level_references stdlib_defs (`Toplevel top_defs) in
    ignore_local_defs stdlib_defs)


let rec patch_refs : def list -> References.references -> def list =
 fun defs refs ->
  let open References in
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
          | Def defs -> Def (patch_refs defs refs)
        in
        let m =
          match LMap.find_opt m.range refs with
          | None -> m
          | Some references -> { m with references }
        in
        Module { m with mod_case })


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
  let stdlib_defs = stdlib_defs ~raise ~options stdlib_core in
  let defs, scopes =
    let `Toplevel top_defs, defs, _, scopes =
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
  (* Use WIP new implementation only during expect_tests *)
  let defs =
    match Sys.getenv "LIGO_GET_SCOPE_USE_NEW_IMP" with
    | Some s when String.(s <> "") ->
      let new_defs = Definitions.definitions prg stdlib_defs in
      let defs = Definitions.Merge_defs_temp.merge_defs defs new_defs in
      let refs = References.declarations (stdlib_core @ prg) in
      patch_refs defs refs
    | Some _ | None -> defs
  in
  defs, scopes
