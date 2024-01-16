open Common
open Lsp_helpers

(* We should handle the case when the given position goes right after the scope.
   Suppose we have:
   {[
     let x =  |
   ]}
   where `|` is a cursor. In scopes the range would be a point right after `=` sign (i.e. [let x =|]).
   So, here we're getting the closest token position to the given one. *)
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


let pick_scope
    ~(definitions : Def.t list)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (scopes : Ligo_interface.scopes)
    (current_file : Path.t)
    : Def.t list option
  =
  let pos = pick_closest_pos cst pos in
  let scope =
    List.find_map scopes ~f:(fun (loc, defs) ->
        let scope_file =
          Def.Def_location.of_loc loc
          |> function
          | File { path; _ } -> Some path
          | StdLib _ | Virtual _ -> None
        in
        match scope_file with
        | Some scope_file when Path.equal scope_file current_file ->
          (match Range.of_loc loc with
          | Some loc when Range.contains_position pos loc -> Some defs
          | _ -> None)
        | _ -> None)
  in
  Option.map
    scope
    ~f:
      (let open Scopes in
      let uid_to_def = Types.Uid_map.of_defs_list definitions in
      Types.fix_shadowing_in_defs
      <@ List.filter_map ~f:(fun uid -> Types.Uid_map.find_opt uid uid_to_def))


(* Definitions that are available at given position.
   Returns [None] if position is not contained in any scopes.
   This can happen only due to bug in scopes, since e.g.stdlib definitions
   should be in all scopes.
   We want to show only completions with non-qualified names, e.g.
   instead of showing [List.map], [List.append], etc we'll just show the [List] module,
   and if user choose [List], the [Fields.get_fields_completions] will show [map], etc *)
let get_defs_completions
    ~(definitions : Def.t list)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (scopes : Ligo_interface.scopes)
    (current_file : Path.t)
    : Def.t list option
  =
  let scope_defs = pick_scope ~definitions cst pos scopes current_file in
  (* Is cursor located in some module? *)
  let module_path =
    let open Cst_shared.Fold in
    List.rev
    @@
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
    List.is_prefix
      ~prefix:Scopes.Types.(List.map ~f:Uid.to_name @@ get_mod_path def)
      ~equal:String.equal
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
    @@ get_defs_completions ~definitions cst pos scopes path
  in
  Common.nub_sort_items with_possible_duplicates
