open Common
open Lsp_helpers

type def_scope =
  | Term_scope
  | Type_scope

let in_scope (def : Def.t) (scope : def_scope) =
  match def, scope with
  | Module _, _ -> true (* We'll show modules both for term and type completions *)
  | Variable _, Term_scope -> true
  | Type _, Type_scope -> true
  | _ -> false


let get_module_from_pos
    ({ path; syntax; definitions; _ } : _ Common.input)
    (scope : def_scope)
    (module_pos : Position.t)
    : CompletionItem.t list option
  =
  let open Option.Monad_infix in
  let rec get_module_defs : Scopes.Types.mod_case -> CompletionItem.t list option
    = function
    | Alias { resolved_module; _ } ->
      Option.bind resolved_module ~f:(fun resolved ->
          List.find_map definitions ~f:(function
              | Variable _ | Type _ -> None
              | Module m ->
                if Scopes.Types.Uid.(m.uid = resolved)
                then get_module_defs m.mod_case
                else None))
    | Def defs ->
      let is_module_field_in_scope def =
        match Def.get_def_type def with
        | Module_field -> in_scope def scope
        | Local | Parameter | Global -> false
      in
      Option.some
      @@ defs_to_completion_items Module_field path syntax
      @@ List.filter ~f:is_module_field_in_scope defs
  in
  Def.get_definition module_pos path definitions
  >>= function
  | Module { mod_case; _ } -> get_module_defs mod_case
  | Variable _ | Type _ -> None


let module_path_impl
    (module_names_before_cursor : lexeme wrap list)
    (input : _ Common.input)
    (scope : def_scope)
    : CompletionItem.t list option
  =
  Option.bind (List.last module_names_before_cursor) ~f:(fun module_name ->
      let module_pos = Position.of_pos module_name#region#start in
      get_module_from_pos input scope module_pos)
