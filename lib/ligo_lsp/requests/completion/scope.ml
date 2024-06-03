open Core
open Common
open Lsp_helpers
module Region = Simple_utils.Region

(** Handle the case when the given position goes right after the scope. Suppose we have:
    {[
      let x =  |
    ]}
    where [|] is a cursor. In scopes, the range would be a point right after [=] sign
    (i.e. [let x =|]). So, here we're getting the closest token position to the given one. *)
let pick_closest_pos (cst : Dialect_cst.t) (pos : Position.t) : Position.t =
  let open Cst_shared.Fold in
  (* Get the closest position *)
  let folder acc cur_pos =
    Option.value_map
      ~default:(Some cur_pos)
      ~f:(fun acc -> if Position.(acc <= cur_pos) then Some cur_pos else Some acc)
      acc
  in
  (* Accept position if it goes before the given *)
  let fold_control reg =
    let open Position in
    let cur_pos = of_pos reg#start in
    if cur_pos <= pos then Continue cur_pos else Stop
  in
  Option.value ~default:pos
  @@
  match cst with
  | CameLIGO cst ->
    let open Cst_cameligo.Fold in
    let collect (Some_node (node, sing)) =
      match sing with
      | S_reg _ -> fold_control node.region
      | S_wrap _ -> fold_control node#region
      | _ -> Skip
    in
    fold_cst None folder collect cst
  | JsLIGO cst ->
    let open Cst_jsligo.Fold in
    let collect (Some_node (node, sing)) =
      match sing with
      | S_reg _ -> fold_control node.region
      | S_wrap _ -> fold_control node#region
      | _ -> Skip
    in
    fold_cst None folder collect cst

(** Traverses the CST spine up to the given position, collecting all module names from the
    declarations that the position is nested in. *)
let get_current_module (cst : Dialect_cst.t) (pos : Position.t) : Scopes.Uid.t list =
  let open Cst_shared.Fold in
  let is_region_of_interest (region : Region.t) : bool =
    Range.(contains_position pos (of_region region))
  in
  List.rev_map ~f:(fun name -> Scopes.Uid.make name#payload (File name#region))
  @@
  match cst with
  | CameLIGO cst ->
    let open Cst_cameligo.Fold in
    let collect module_path (Some_node (node, sing)) =
      match sing with
      | S_reg S_module_decl when is_region_of_interest node.region ->
        Continue (node.value.name :: module_path)
      | _ -> Skip
    in
    fold_cst' [] collect cst
  | JsLIGO cst ->
    let open Cst_jsligo.Fold in
    let collect module_path (Some_node (node, sing)) =
      match sing with
      | S_reg S_namespace_decl when is_region_of_interest node.region ->
        Continue (node.value.namespace_name :: module_path)
      | _ -> Skip
    in
    fold_cst' [] collect cst

(** Gets all definitions that are available at given position.

    We want to show only completions with non-qualified names, e.g. instead of showing
    [List.map], [List.append], etc, we'll just show the [List] module, and if the user
    wrote [List.], the [Fields.get_fields_completions] will show [map], etc. *)
let get_defs_completions
    ~(normalize : Path.normalization)
    ({ path; syntax = _; definitions = _; cst; pos } : _ Common.input)
    (hierarchy : Def.Hierarchy.t)
    : Def.t list
  =
  let mod_path = get_current_module cst pos in
  let pos = pick_closest_pos cst pos in
  Def.Hierarchy.scope_at_point ~normalize path pos mod_path hierarchy

let get_scope_completions
    ~(normalize : Path.normalization)
    ({ path; syntax; definitions = _; cst = _; pos = _ } as input : _ Common.input)
    (hierarchy : Def.Hierarchy.t)
    : CompletionItem.t list
  =
  let with_possible_duplicates =
    defs_to_completion_items ~normalize Scope path syntax
    @@ get_defs_completions ~normalize input hierarchy
  in
  Common.nub_sort_items with_possible_duplicates
