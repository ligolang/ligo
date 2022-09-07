open New_types

module AST = Ast_core

module VVar = AST.ValueVar
module TVar = AST.TypeVar
module MVar = AST.ModuleVar

(* module Formatter = Formatter *)
(* module Api_helper = Api_helper *)

module Misc = New_misc

type typing_env = { type_env : Environment.t  ; bindings : Misc.bindings_map }

let update_typing_env : with_types:bool -> options:Compiler_options.middle_end -> typing_env -> AST.declaration -> typing_env
  = fun ~with_types ~options tenv decl ->
      match with_types with
        true ->
          begin
            let typed_prg = Simple_utils.Trace.to_option @@
              Checking.type_declaration ~options ~env:tenv.type_env decl in
              match typed_prg with
              | Some decl ->
                let module AST = Ast_typed in
                let bindings = Misc.extract_variable_types tenv.bindings decl.wrap_content in
                let type_env = Environment.add_declaration decl tenv.type_env in
                { type_env ; bindings }
              | None -> tenv
          end
      | false -> tenv

type def_list = (string * def) list

type reference =
    Variable of AST.expression_variable
  | ModuleAccess of AST.module_variable list * AST.expression_variable
  | ModuleAlias of AST.module_variable list

let pp_reference : Format.formatter -> reference -> unit
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
      List.iter refs ~f:(pp_reference f)

let get_location_of_module_path : MVar.t list -> Location.t
  = fun mvs ->
      List.fold mvs ~init:(Location.dummy)
        ~f:(fun loc m -> Location.cover loc (MVar.get_location m))

module Free = struct

  let rec update_variable_reference : AST.expression_variable -> def list -> bool * def list
    = fun ev defs ->
        match defs with
          [] -> false, []
        | Variable v::defs when VVar.is_name ev v.name ->
          let loc = VVar.get_location ev in
          let references = loc :: v.references in
          true, Variable { v with references } :: defs
        | def::defs ->
          let updated, defs = update_variable_reference ev defs in
          updated, def :: defs

  let rec update_module_variable_references : AST.module_variable list -> AST.expression_variable option -> def list -> bool * def list
    = fun mvs ev defs ->
        match mvs, defs with
          _, [] -> false, defs
        | [], defs ->
          begin match ev with
            Some ev -> update_variable_reference ev defs
          | None -> true, defs
          end
        | mv::mvs, Module ({ name ; mod_case = Def d ; _ } as m)::defs when MVar.is_name mv name ->
            let loc = MVar.get_location mv in
            let references = loc :: m.references in
            let updated, d  = update_module_variable_references mvs ev d in
            let mod_case = Def d in
            updated, Module { m with mod_case ; references } :: defs
        | mv::mvs, Module ({ name ; mod_case = Alias a ; _ } as m)::defs when MVar.is_name mv name ->
            let loc = MVar.get_location mv in
            let references = loc :: m.references in
            let updated, defs = resolve_alias a mvs ev defs in
            updated, Module { m with references } :: defs
        | mvs, def::defs ->
            let updated, defs = update_module_variable_references mvs ev defs in
            updated, def :: defs
    and resolve_alias : string list -> AST.module_variable list -> AST.expression_variable option -> def list -> bool * def list
      = fun aliases mvs ev defs ->
          match aliases with
          | [] -> update_module_variable_references mvs ev defs
          | alias::aliases ->
              let rec aux = function
                [] -> false, []
              | Module ({ name ; mod_case = Def d ; _ } as m)::defs when String.(name = alias) ->
                  let updated, d = resolve_alias aliases mvs ev d in
                  let mod_case = Def d in
                  updated, Module { m with mod_case } :: defs
              | Module ({ name ; mod_case = Alias a ; _ }) as def::defs when String.(name = alias) ->
                let updated, defs = resolve_alias (a @ aliases) mvs ev defs in
                updated, def :: defs
              | def::defs ->
                  let updated, defs = aux defs in
                  updated, def :: defs
              in
              aux defs

  let update_reference : reference -> def list -> bool * def list
    = fun r defs ->
        match r with
          Variable ev -> update_variable_reference ev defs
        | ModuleAccess (mvs, ev) -> update_module_variable_references mvs (Some ev) defs
        | ModuleAlias mvs -> update_module_variable_references mvs None defs

  let update_references : reference list -> def list -> def list * reference list
    = fun refs defs ->
        let defs, refs = List.fold_left refs ~init:(defs, [])
        ~f:(fun (defs, refs) r ->
          let updated, defs = update_reference r defs in
          let refs = if updated then refs else r :: refs in
          defs, refs
        ) in
        defs, refs

  let rec expression : with_types:bool -> options:Compiler_options.middle_end -> typing_env -> AST.expression -> def list * reference list * typing_env * scopes
    = fun ~with_types ~options tenv e ->
        let expression = expression ~with_types ~options in
        match e.expression_content with
          E_literal _ -> [], [], tenv, [e.location, []]
        | E_raw_code _ -> [], [], tenv, [e.location, []]
        | E_variable ev -> [], [Variable ev], tenv, [e.location, []]
        | E_module_accessor m -> [], [ModuleAccess (m.module_path, m.element)], tenv, [e.location, []]
        | E_constant { arguments ; _ } ->
          List.fold_left arguments ~init:([], [], tenv, [])
            ~f:(fun (defs, refs, tenv, scopes) e ->
                  let ds, rs, tenv, scopes' = expression tenv e in
                  ds @ defs, rs @ refs, tenv, scopes @ scopes')
        | E_application { lamb ; args } ->
          let defs, refs, tenv, scopes = expression tenv lamb  in
          let defs', refs', tenv, scopes' = expression tenv args in
          defs' @ defs, refs' @ refs, tenv, scopes @ scopes'
        | E_lambda { binder = { var ; ascr = core_type ; _ } ; result ; output_type = _ } ->
          let def =
            let binder_loc =  VVar.get_location var in
            Misc.make_v_def ~with_types ?core_type tenv.bindings var binder_loc result.location
          in
          let defs_result, refs_result, tenv, scopes = expression tenv result in
          defs_result @ [def], refs_result, tenv, scopes
        | E_type_abstraction { result ; _ } -> expression tenv result
        | E_constructor { element ; _ } -> expression tenv element
        | E_record_accessor { record ; _ } -> expression tenv record
        | E_ascription { anno_expr ; _ } -> expression tenv anno_expr
        | E_record_update { record ; update ; _ } ->
          let defs, refs, tenv, scopes =  expression tenv record in
          let defs', refs', tenv, scopes' = expression tenv update in
          defs' @ defs, refs' @ refs, tenv, scopes @ scopes'
        | E_record e_lable_map ->
          let defs, refs, tenv, scopes = AST.LMap.fold (fun _ e (defs, refs, tenv, scopes) ->
            let defs', refs', tenv, scopes' = expression tenv e in
            defs' @ defs, refs' @ refs, tenv, scopes @ scopes'
          ) e_lable_map ([], [], tenv, []) in
          defs, refs, tenv, scopes
        | E_assign { binder ; expression = e } ->
          let refs' = [Variable binder.var] in
          let defs, refs, tenv, scopes = expression tenv e in
          defs, refs @ refs', tenv, scopes
        | E_let_in { let_binder = { var ; ascr = core_type ; _ } ; rhs ; let_result ; _ } ->
          let def =
            (* let binder_name = get_binder_name let_binder.var in *)
            let binder_loc =  VVar.get_location var in
            Misc.make_v_def ~with_types ?core_type tenv.bindings var binder_loc rhs.location
          in
          let defs_rhs, refs_rhs, tenv, scopes = expression tenv rhs in
          let defs_result, refs_result, tenv, scopes' = expression tenv let_result in
          let scopes = scopes @ scopes' in
          defs_result @ defs_rhs @ [def], refs_result @ refs_rhs, tenv, scopes
        | E_recursive { fun_name ; fun_type ; lambda = { binder = { var ; ascr = core_type ; _ } ; result ; _ } } ->
          let def_fun =
            let binder_loc =  VVar.get_location fun_name in
            Misc.make_v_def ~with_types ~core_type:fun_type tenv.bindings fun_name binder_loc (result.location)
          in
          let def_par =
            (* let binder_name = get_binder_name binder.var in *)
            let binder_loc =  VVar.get_location var in
            Misc.make_v_def ~with_types ?core_type tenv.bindings var binder_loc (result.location)
          in
          let defs = [def_fun ; def_par] in
          let defs_result, refs_result, tenv, scopes = expression tenv result in
          defs_result @ defs, refs_result, tenv, scopes
        | E_type_in { type_binder ; rhs ; let_result } ->
          let def = type_expression type_binder rhs in
          let defs, refs, tenv, scopes = expression tenv let_result in
          [def] @ defs, refs, tenv, scopes
        | E_matching { matchee ; cases } ->
          let defs_matchee, refs_matchee, tenv, scopes = expression tenv matchee in
          let defs_cases, refs_cases, tenv, scopes' = List.fold_left cases ~init:([], [], tenv, [])
            ~f:(fun (defs, refs, tenv, scopes') { pattern ; body } ->
              let defs_pat = Stage_common.Helpers.fold_pattern (
                fun defs (p : _ AST.pattern) ->
                  match p.wrap_content with
                    P_var { var ; ascr = core_type ; _ } ->
                      let def =
                        (* let binder_name = get_binder_name binder.var in *)
                        let binder_loc =  VVar.get_location var in
                        Misc.make_v_def ~with_types ?core_type tenv.bindings var binder_loc body.location
                      in
                      def :: defs
                  | _ -> defs
              ) [] pattern in
              let defs_body, refs_body, tenv, scopes = expression tenv body in
              defs_body @ defs_pat @ defs, refs_body @ refs, tenv, scopes @ scopes'
            )
          in
          defs_matchee @ defs_cases, refs_matchee @ refs_cases, tenv, scopes @ scopes'
        | E_mod_in { module_binder ; rhs ; let_result } ->
          let defs_module, refs_module, tenv, scopes = module_expression ~with_types ~options tenv module_binder rhs in
          let defs_result, refs_result, tenv, scopes' = expression tenv let_result in
          defs_result @ defs_module, refs_result @ refs_module, tenv, scopes @ scopes'
    and type_expression : TVar.t -> AST.type_expression -> def
      = fun tv t ->
          let def =
            let binder_name = get_type_binder_name tv in
            let binder_loc =  TVar.get_location tv in
            make_t_def binder_name binder_loc t
          in
          def
    and module_expression : with_types:bool -> options:Compiler_options.middle_end -> typing_env -> MVar.t -> AST.module_expr -> def list * reference list * typing_env * scopes
      = fun ~with_types ~options tenv top m ->
          match m.wrap_content with
            M_struct decls ->
              let defs, refs, tenv, scopes = declarations ~with_types ~options tenv decls in
              let range = MVar.get_location top in
              let body_range = m.location in
              let name = get_mod_binder_name top in
              let def = make_m_def ~range ~body_range name defs in
              [def], refs, tenv, scopes
          | M_variable mv ->
            let def, reference =
              let name = get_mod_binder_name top in
              let range =  MVar.get_location top in
              let body_range = MVar.get_location mv in
              let alias = [get_mod_binder_name mv] in
              let def = make_m_alias_def ~range ~body_range name alias in
              let reference = ModuleAlias [mv] in
              def, reference
            in
            [def], [reference], tenv, []
          | M_module_path path ->
            let mvs = List.Ne.to_list path in
            let def, reference =
              let name = get_mod_binder_name top in
              let range =  MVar.get_location top in
              let body_range = get_location_of_module_path mvs in
              let alias = List.map mvs ~f:get_mod_binder_name in
              let def = make_m_alias_def ~range ~body_range name alias in
              let reference = ModuleAlias mvs in
              def, reference
            in
            [def], [reference], tenv, []
    and declaration_expression : with_types:bool -> options:Compiler_options.middle_end -> ?core_type:AST.type_expression -> typing_env -> VVar.t -> AST.expression -> def list * reference list * typing_env * scopes
      = fun ~with_types ~options ?core_type tenv ev e ->
          let defs, refs, tenv, scopes = expression ~with_types ~options tenv e in
          (* let name = get_binder_name ev in *)
          let range = VVar.get_location ev in
          let body_range = e.location in
          (* TODO: clean up *)
          let def = Misc.make_v_def ~with_types ?core_type tenv.bindings ev range body_range in
          [def] @ defs, refs, tenv, scopes
    and declaration : with_types:bool -> options:Compiler_options.middle_end -> typing_env -> AST.declaration -> def list * reference list * typing_env * scopes
      = fun ~with_types ~options tenv decl ->
        let tenv = update_typing_env ~with_types ~options tenv decl in
        match decl.wrap_content with
          Declaration_constant { attr        = { hidden ; _ } ; _ }
        | Declaration_module   { module_attr = { hidden ; _ } ; _ }
        | Declaration_type     { type_attr   = { hidden ; _ } ; _ } when hidden -> [], [], tenv, []
        | Declaration_constant { binder      = { var ; ascr = core_type ; _ } ; expr ; _ } ->
          (* TODO: use ascr *)
          declaration_expression ~with_types ~options ?core_type tenv var expr
        | Declaration_type     { type_binder ; type_expr ; _ } ->
          let def = type_expression type_binder type_expr in
          [def], [], tenv, []
        | Declaration_module   { module_binder ; module_ ; _ } ->
          (* let tenv = update_typing_env ~with_types ~options tenv decl in  *)
          module_expression ~with_types ~options tenv module_binder module_
    and declarations : with_types:bool -> options:Compiler_options.middle_end -> typing_env -> _ AST.declarations' -> def list * reference list * typing_env * scopes
      = fun ~with_types ~options tenv decls ->
          let defs, refs, tenv, scopes = List.fold_left decls ~init:([], [], tenv, [])
            ~f:(fun (defs, refs, tenv, scopes') decl ->
              let defs', refs', tenv, scopes = declaration ~with_types ~options tenv decl in
              let defs, refs = update_references (refs' @ refs) (defs' @ defs) in
              let scopes = add_defs_to_scopes defs scopes in
              defs, refs, tenv, scopes @ scopes')
          in
          defs, refs, tenv, scopes
end

module Def_map = Types.Def_map
let rec to_def_map : def list -> Types.def_map
  = fun defs ->
      let defs = List.rev defs in
      List.fold_left defs ~init:Def_map.empty
        ~f:(fun def_map def ->
          match def with
            Variable v ->
              let def_name = Misc.make_def_id v.name in
              let t = match v.t with 
                New_types.Unresolved -> Types.Unresolved 
              | Core t -> Types.Core t
              | Resolved t -> Types.Resolved t
              in
              let def = Types.Variable {
                name  = v.name ;
                range = v.range ;
                body_range = v.body_range ;
                t ;
                references = v.references ;
              } in
              Def_map.add def_name def def_map
          | Type t ->
              let def_name = Misc.make_def_id t.name in
              let def = Types.Type t in
              Def_map.add def_name def def_map
          | Module ({ mod_case = Alias a } as m) ->
              let def_name = Misc.make_def_id m.name in
              let mod_case = Types.Alias a in
              let def = Types.Module {
                name = m.name ;
                range = m.range ;
                body_range = m.body_range ;
                references = m.references ;
                mod_case ;
              } in
              Def_map.add def_name def def_map
          | Module ({ mod_case = Def d } as m) ->
              let def_name = Misc.make_def_id m.name in
              let mod_case = Types.Def (to_def_map d) in
              let def = Types.Module {
                name = m.name ;
                range = m.range ;
                body_range = m.body_range ;
                references = m.references ;
                mod_case ;
              } in
              Def_map.add def_name def def_map
        )
let scopes : with_types:bool -> options:Compiler_options.middle_end -> AST.module_ -> (def list * scopes)
  = fun ~with_types ~options prg ->
      let tenv = { type_env = options.init_env ; bindings = Misc.Bindings_map.empty } in
      let defs, _refs, _, scopes = Free.declarations ~with_types ~options tenv prg in
      let def_map = to_def_map defs in
      let () = Format.printf "----------------------------------\n" in
      let () = Format.printf "%a\n" PP.definitions def_map in
      let () = Format.printf "----------------------------------\n" in
      let () = Format.printf "++++++++++++++++++++++++++++++++++\n" in
      let () = List.iter ~f:(fun (loc, defs) ->
        let bs = List.map defs ~f:get_def_name in
        let bs = String.concat ~sep:", " bs in
        Format.printf "[%s] %a\n" bs Location.pp loc  
      ) scopes in
      let () = Format.printf "++++++++++++++++++++++++++++++++++\n" in
      [], scopes

(*

For each declaration there will be a def
a function on expression will returns def list & references (vars) list

for an expression its free_variable will be references


4. next add scopes
5. Add comments

*)