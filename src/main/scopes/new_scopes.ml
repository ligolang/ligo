open New_types

module AST = Ast_core

module VVar = AST.ValueVar
module TVar = AST.TypeVar
module MVar = AST.ModuleVar

(* module Formatter = Formatter *)
(* module Api_helper = Api_helper *)

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
            (* Format.printf "update_module_variable_references Def %s\n" name; *)
            let loc = MVar.get_location mv in
            let references = loc :: m.references in
            let updated, d  = update_module_variable_references mvs ev d in
            let mod_case = Def d in
            updated, Module { m with mod_case ; references } :: defs
        | mv::mvs, Module ({ name ; mod_case = Alias a ; _ } as m)::defs when MVar.is_name mv name ->
          (* Format.printf "update_module_variable_references Alias %s - %s\n" name (String.concat ~sep:"." a); *)
            let loc = MVar.get_location mv in
            let references = loc :: m.references in
            let updated, defs = resolve_alias a mvs ev defs in
            updated, Module { m with references } :: defs
        | mvs, def::defs ->
          (* Format.printf "update_module_variable_references :( %s\n" (get_def_name def); *)
            let updated, defs = update_module_variable_references mvs ev defs in
            updated, def :: defs
    and resolve_alias : string list -> AST.module_variable list -> AST.expression_variable option -> def list -> bool * def list
      = fun aliases mvs ev defs ->
        (* Format.printf "->> resolve_alias %s  \n" (String.concat ~sep:"." aliases); *)
        (* Format.printf "->> defs - [%s]" (String.concat ~sep:", " (List.map defs ~f:get_def_name)); *)
          match aliases with
          | [] -> update_module_variable_references mvs ev defs
          | alias::aliases ->
              let rec aux = function
                [] -> false, []
              | Module ({ name ; mod_case = Def d ; _ } as m)::defs when String.(name = alias) ->
                (* Format.printf "resolve_alias Def %s\n" name; *)
                  let updated, d = resolve_alias aliases mvs ev d in
                  let mod_case = Def d in
                  updated, Module { m with mod_case } :: defs
              | Module ({ name ; mod_case = Alias a ; _ }) as def::defs when String.(name = alias) ->
                (* Format.printf "resolve_alias Alias %s - %s\n" name (String.concat ~sep:"." a); *)
                let updated, defs = resolve_alias (a @ aliases) mvs ev defs in
                updated, def :: defs
              | def::defs ->
                (* Format.printf "resolve_alias :( %s\n" (get_def_name def); *)
                  let updated, defs = aux defs in
                  updated, def :: defs
              in
              aux defs

  let update_reference : reference -> def list -> bool * def list
    = fun r defs ->
      (* Format.printf "\n------------------ %a --------------\n" pp_reference r; *)
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

  let rec expression : AST.expression -> def list * reference list
    = fun e ->
        match e.expression_content with
          E_literal _ -> [], []
        | E_raw_code _ -> [], []
        | E_variable ev -> [], [Variable ev]
        | E_module_accessor m -> [], [ModuleAccess (m.module_path, m.element)]
        | E_constant { arguments ; _ } ->
          List.fold_left arguments ~init:([], [])
            ~f:(fun (defs, refs) e ->
                  let ds, rs = expression e in
                  ds @ defs, rs @ refs)
        | E_application { lamb ; args } ->
          let defs, refs = expression lamb  in
          let defs', refs' = expression args in
          defs' @ defs, refs' @ refs
        | E_lambda { binder ; result ; output_type = _ } ->
          (* TODO: handle input_type *)
          (* TODO: handle output_type *)
          let def =
            let binder_name = get_binder_name binder.var in
            let binder_loc =  VVar.get_location binder.var in
            make_v_def binder_name Unresolved binder_loc (result.location)
          in
          let defs, refs = expression result in
          let defs = [def] @ defs in
          let defs, refs = update_references refs defs in
          defs, refs
        | E_type_abstraction { result ; _ } -> expression result
        | E_constructor { element ; _ } -> expression element
        | E_record_accessor { record ; _ } -> expression record
        | E_ascription { anno_expr ; _ } -> expression anno_expr
        | E_record_update { record ; update ; _ } ->
          let defs, refs =  expression record in
          let defs', refs' = expression update in
          defs' @ defs, refs' @ refs
        | E_record e_lable_map ->
          let defs, refs = AST.LMap.fold (fun _ e (defs, refs) ->
            let defs', refs' = expression e in
            defs' @ defs, refs' @ refs
          ) e_lable_map ([], []) in
          defs, refs
        | E_assign { binder ; expression = e } ->
          let refs' = [Variable binder.var] in
          let defs, refs = expression e in
          defs, refs @ refs'
        | E_let_in { let_binder ; rhs ; let_result ; _ } ->
          let def =
            let binder_name = get_binder_name let_binder.var in
            let binder_loc =  VVar.get_location let_binder.var in
            make_v_def binder_name Unresolved binder_loc (rhs.location)
          in
          let defs_rhs, refs_rhs = expression rhs in
          let defs_result, refs_result = expression let_result in
          let defs, refs_result = update_references refs_result [def] in
          defs_result @ defs_rhs @ defs, refs_result @ refs_rhs
        | E_recursive { fun_name ; fun_type ; lambda = { binder ; result ; _ } } ->
          (* TODO: handle input_type *)
          let def_fun =
            let binder_name = get_binder_name fun_name in
            let binder_loc =  VVar.get_location fun_name in
            make_v_def binder_name (Core fun_type) binder_loc (result.location)
          in
          let def_par =
            let binder_name = get_binder_name binder.var in
            let binder_loc =  VVar.get_location binder.var in
            make_v_def binder_name Unresolved binder_loc (result.location)
          in
          let defs = [def_fun ; def_par] in
          let defs_result, refs_result = expression result in
          let defs, refs_result = update_references refs_result defs in
          defs_result @ defs, refs_result
        | E_type_in { type_binder ; rhs ; let_result } ->
          let def = type_expression type_binder rhs in
          let defs, refs = expression let_result in
          [def] @ defs, refs
        | E_matching { matchee ; cases } ->
          let defs_matchee, refs_matchee = expression matchee in
          let defs_cases, refs_cases = List.fold_left cases ~init:([], [])
            ~f:(fun (defs, refs) { pattern ; body } ->
              let defs_pat = Stage_common.Helpers.fold_pattern (
                fun defs (p : _ AST.pattern) ->
                  match p.wrap_content with
                    P_var binder ->
                      let def =
                        let binder_name = get_binder_name binder.var in
                        let binder_loc =  VVar.get_location binder.var in
                        make_v_def binder_name Unresolved binder_loc (body.location)
                      in
                      def :: defs
                  | _ -> defs
              ) [] pattern in
              let defs_body, refs_body = expression body in
              let defs_pat, refs_body = update_references refs_body defs_pat in
              defs_body @ defs_pat @ defs, refs_body @ refs
            )
          in
          defs_matchee @ defs_cases, refs_matchee @ refs_cases
        | E_mod_in { module_binder ; rhs ; let_result } -> [], [] (* TODO: implement this *)
    and type_expression : TVar.t -> AST.type_expression -> def
      = fun tv t ->
          let def =
            let binder_name = get_type_binder_name tv in
            let binder_loc =  TVar.get_location tv in
            make_t_def binder_name binder_loc t
          in
          def
    and module_expression : MVar.t -> AST.module_expr -> def list * reference list
      = fun top m ->
          match m.wrap_content with
            M_struct decls ->
              let defs, refs = declarations decls in
              let range = MVar.get_location top in
              let body_range = m.location in
              let name = get_mod_binder_name top in
              let def = make_m_def ~range ~body_range name defs in
              [def], refs
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
            [def], [reference]
          | M_module_path path ->
            let mvs = List.Ne.to_list path in
            let def, reference =
              let name = get_mod_binder_name top in
              let range =  MVar.get_location top in
              let body_range = m.location in
              let alias = List.map mvs ~f:get_mod_binder_name in
              let def = make_m_alias_def ~range ~body_range name alias in
              let reference = ModuleAlias mvs in
              def, reference
            in
            [def], [reference]
    and declaration_expression : VVar.t -> AST.expression -> def list * reference list
      = fun ev e ->
          let defs, refs = expression e in
          let name = get_binder_name ev in
          let range = VVar.get_location ev in
          let body_range = e.location in
          let def = make_v_def name Unresolved range body_range in
          [def] @ defs, refs
    and declaration : AST.declaration -> def list * reference list
      = fun decl ->
        match decl.wrap_content with
          Declaration_constant { attr        = { hidden ; _ } ; _ }
        | Declaration_module   { module_attr = { hidden ; _ } ; _ }
        | Declaration_type     { type_attr   = { hidden ; _ } ; _ } when hidden -> [], []
        | Declaration_constant { binder      = { var ; ascr=_ ; _ } ; expr ; _ } ->
          (* TODO: use ascr *)
          declaration_expression var expr
        | Declaration_type     { type_binder ; type_expr ; _ } ->
          let def = type_expression type_binder type_expr in
          [def], []
        | Declaration_module   { module_binder ; module_ ; _ } ->
          module_expression module_binder module_
    and declarations : _ AST.declarations' -> def list * reference list
      = fun decls ->
          let defs, refs = List.fold_left decls ~init:([], [])
            ~f:(fun (defs, refs) decl ->
              let defs', refs' = declaration decl in
              let defs, refs = update_references (refs' @ refs) (defs' @ defs) in
              (* Format.printf "^^^^^^^^^^^^^^ defs - [%s]\n" (String.concat ~sep:", " (List.map defs ~f:get_def_name)); *)
              (* let () = List.iter defs ~f:(fun d -> Format.printf "BBBBBB %s\n" (get_def_name d)) in *)
              (* let () = List.iter refs ~f:(fun r -> Format.printf "CCCCCC %a\n" pp_reference r) in *)
              defs, refs)
          in
          defs, refs
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
              let def = Types.Variable v in
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
      let defs, _refs = Free.declarations prg in
      let def_map = to_def_map defs in
      let () = Format.printf "----------------------------------\n" in
      let () = Format.printf "%a\n" PP.definitions def_map in
      let () = Format.printf "----------------------------------\n" in
      [], []

(*

For each declaration there will be a def
a function on expression will returns def list & references (vars) list

for an expression its free_variable will be references

Initial version only defintions no types
next add types
next add scopes

*)