open Ast_unified
open Pass_type
open Errors
open Simple_utils.Trace
module Location = Simple_utils.Location

(* A series of heuristics for patterns, focused on JsLIGO patterns *)
include Flag.No_arg ()

let rec map_pattern ~f (p : pattern) : pattern =
  let continue, p = f p in
  let self = if continue then map_pattern ~f else Fun.id in
  let loc, p = destruct_p p in
  let p = map_pattern_ self Fun.id (Location.wrap ~loc p) in
  make_p ~loc (Location.unwrap p)


(* These are a series of passes that try some heuristics on patterns *)

(* Turn a tuple to unit *)
let tuple_to_unit (p : pattern) =
  let loc, p = destruct_p p in
  match p with
  | P_tuple_with_ellipsis [] -> false, make_p ~loc P_unit
  | P_typed (t, p) -> false, make_p ~loc (P_typed (t, p))
  | _ -> true, make_p ~loc p


(* Some heuristic for lists *)
let tuple_to_list (p : pattern) =
  let loc, p = destruct_p p in
  let rec finishes_in_ellipsis (ps : pattern element_pattern list)
      : pattern List.Ne.t option
    =
    match ps with
    | [] -> None
    | [ { pattern; ellipsis = true } ] -> Some (List.Ne.singleton pattern)
    | { pattern; ellipsis = false } :: tl ->
      let open Simple_utils.Option in
      let* ps = finishes_in_ellipsis tl in
      Some (List.Ne.cons pattern ps)
    | _ -> None
  in
  match p with
  | P_tuple_with_ellipsis [] -> true, make_p ~loc (P_list (List []))
  | P_tuple_with_ellipsis ps ->
    (match finishes_in_ellipsis ps with
    | None -> true, make_p ~loc p
    | Some ps ->
      let list_pattern =
        List.Ne.fold_right1 ps ~f:(fun p q -> make_p ~loc (P_list (Cons (p, q))))
      in
      true, list_pattern)
  | P_typed (t, p) -> false, make_p ~loc (P_typed (t, p))
  | _ -> true, make_p ~loc p


(* Replace all ellipsis for tuple *)
let remove_ellipsis ~raise (p : pattern) =
  let loc, p = destruct_p p in
  match p with
  | P_tuple_with_ellipsis l ->
    let l =
      List.map
        ~f:(fun { pattern; ellipsis } ->
          if ellipsis
          then raise.error (invalid_list_pattern_match (get_p_loc pattern))
          else pattern)
        l
    in
    true, make_p ~loc (P_tuple l)
  | _ -> true, make_p ~loc p


let compile ~raise =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    let p = make_p ~loc (Location.unwrap p) in
    let p = map_pattern ~f:tuple_to_list p in
    let p = map_pattern ~f:(remove_ellipsis ~raise) p in
    p
  in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_match_tc39
        { subject; match_clauses = AllClauses (({ filter; clause_expr }, []), None) } ->
      let clause =
        Match_tc39.{ filter = map_pattern ~f:tuple_to_unit filter; clause_expr }
      in
      e_match_tc39 ~loc { subject; match_clauses = AllClauses ((clause, []), None) }
    | _ -> same
  in
  Fold { idle_fold with pattern; expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
