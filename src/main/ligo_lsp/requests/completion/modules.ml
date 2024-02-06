open Common
open Lsp_helpers

type def_scope =
  | Term_scope
  | Type_scope
  | Module_scope

let in_scope (def : Def.t) (scope : def_scope) =
  match def, scope with
  | Module _, (Term_scope | Type_scope | Module_scope) ->
    true (* We'll show modules for all definition scopes *)
  | Variable _, Term_scope -> true
  | Type _, Type_scope -> true
  | _ -> false


let get_module_from_pos
    ({ path; syntax; definitions; _ } : _ Common.input)
    (scope : def_scope)
    (module_pos : Position.t)
    : CompletionItem.t list option
  =
  let rec get_defs_from_resolve_mod_name
      : Scopes.Types.resolve_mod_name -> CompletionItem.t list option
    = function
    | Unresolved_path { module_path = _ } -> None
    | Resolved_path { module_path = _; resolved_module_path = _; resolved_module } ->
      Def.find_map definitions ~f:(function
          | Variable _ | Type _ | Label _ -> None
          | Module m ->
            if Scopes.Uid.(m.uid = resolved_module) then get_defs_from_mdef m else None)
  and get_defs_from_mdef (m : Scopes.Types.mdef) : CompletionItem.t list option =
    let%map.Option defs =
      match m.mod_case with
      | Alias { resolve_mod_name } -> get_defs_from_resolve_mod_name resolve_mod_name
      | Def defs ->
        let is_module_field_in_scope def =
          match Scopes.Types.get_def_type def with
          | Module_field -> in_scope def scope
          | Local | Parameter | Global -> false
        in
        Option.some
        @@ defs_to_completion_items Module_field path syntax
        @@ List.filter ~f:is_module_field_in_scope defs
    in
    let super_defs = List.filter_map ~f:get_defs_from_resolve_mod_name m.extends in
    List.concat (defs :: super_defs)
  in
  match%bind.Option Def.get_definition module_pos path definitions with
  | Module m -> get_defs_from_mdef m
  | Variable _ | Type _ | Label _ -> None


let module_path_impl
    (module_names_before_cursor : lexeme wrap list)
    (input : _ Common.input)
    (scope : def_scope)
    : CompletionItem.t list option
  =
  Option.bind (List.last module_names_before_cursor) ~f:(fun module_name ->
      let module_pos = Position.of_pos module_name#region#start in
      get_module_from_pos input scope module_pos)
