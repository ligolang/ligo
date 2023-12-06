open Common
open Lsp_helpers

(* Definitions that are available at given position.
   Returns [None] if position is not contained in any scopes.
   This can happen only due to bug in scopes, since e.g.stdlib definitions
   should be in all scopes.
   We want to show only completions with non-qualified names, e.g.
   instead of showing [List.map], [List.append], etc we'll just show the [List] module,
   and if user choose [List], the [Fields.get_fields_completions] will show [map], etc *)
let get_defs_completions
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (scopes : Ligo_interface.scopes)
    : Def.t list option
  =
  let scope_defs =
    List.find_map scopes ~f:(fun (loc, defs) ->
        match Range.of_loc loc with
        | Some loc when Range.contains_position pos loc -> Some defs
        | _ -> None)
  in
  (* Is cursor located in some module? *)
  let module_path =
    let open Cst_shared.Fold in
    match cst with
    | CameLIGO cst ->
      let open Cst_cameligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_module_decl when Range.(contains_position pos (of_region node.region))
          -> Continue node.value.name
        | _ -> Skip
      in
      fold_cst [] (Fn.flip List.cons) collect cst
    | JsLIGO cst ->
      let open Cst_jsligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_namespace_decl
          when Range.(contains_position pos (of_region node.region)) ->
          Continue node.value.namespace_name
        | _ -> Skip
      in
      fold_cst [] (Fn.flip List.cons) collect cst
  in
  (* We want to show [M1.M2.x] in completions only if cursor is located in [M1.M2]
     (or [M1.M2.M3]), since if we're at toplevel we're already showing [M1] *)
  let available_in_current_module (def : Def.t) : bool =
    List.is_prefix ~prefix:(Def.get_mod_path def) ~equal:String.equal
    @@ List.map ~f:(fun x -> x#payload) module_path
  in
  Option.map ~f:(List.filter ~f:available_in_current_module) scope_defs


let get_scope_completions
    ({ path; syntax; definitions; cst; pos } : _ Common.input)
    (scopes : Ligo_interface.scopes)
    : CompletionItem.t list
  =
  (* TODO: In case we found [None], let's at least show the entire scope to
     the user so the completions aren't empty. This happens because scopes
     aren't accurate and may be missing on some ranges. As soon as scopes are
     improved, we should remove this workaround. *)
  let with_possible_duplicates =
    defs_to_completion_items Scope path syntax
    @@ Option.value ~default:definitions
    @@ get_defs_completions cst pos scopes
  in
  Common.nub_sort_items with_possible_duplicates
