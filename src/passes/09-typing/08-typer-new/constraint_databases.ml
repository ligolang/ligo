module Map = RedBlackTrees.PolyMap
module UF = UnionFind.Poly2
open Ast_typed.Types

(* Light wrapper for API for grouped_by_variable in the structured
   db, to access it modulo unification variable aliases. *)
let get_constraints_related_to : type_variable -> structured_dbs -> constraints =
  fun variable dbs ->
    (* get the class of the variable *)
    let variable_repr , _ = UF.get_or_set variable dbs.aliases in
    match Map.find_opt variable_repr dbs.grouped_by_variable with
      Some l -> l
    | None -> {
        constructor = [] ;
        poly        = [] ;
        tc          = [] ;
        row         = [] ;
      }

let add_constraints_related_to : type_variable -> constraints -> structured_dbs -> structured_dbs =
  fun variable c dbs ->
    (* let (variable_repr , _height) , aliases = UF.get_or_set variable dbs.aliases in
       let dbs = { dbs with aliases } in *)
    let variable_repr , aliases = UF.get_or_set variable dbs.aliases in
    let dbs = { dbs with aliases } in
    let grouped_by_variable = Map.update variable_repr (function
          None -> Some c
        | Some (x : constraints) -> Some {
            constructor = c.constructor @ x.constructor ;
            poly        = c.poly        @ x.poly        ;
            tc          = c.tc          @ x.tc          ;
            row         = c.row         @ x.row         ;
          })
        dbs.grouped_by_variable
    in
    let dbs = { dbs with grouped_by_variable } in
    dbs

let merge_constraints : type_variable -> type_variable -> structured_dbs -> structured_dbs =
  fun variable_a variable_b dbs ->
  (* get old representant for variable_a *)
  let variable_repr_a , aliases = UF.get_or_set variable_a dbs.aliases in
  (* get old representant for variable_b *)
  let variable_repr_b , aliases = UF.get_or_set variable_b aliases in

  (* alias variable_a and variable_b together *)
  let aliases = UF.alias variable_a variable_b aliases in
  let dbs = { dbs with aliases } in

  (* Replace the two entries in grouped_by_variable by a single one *)
  let get_constraints ab =
    match Map.find_opt ab dbs.grouped_by_variable with
    | Some x -> x
    | None -> { constructor = [] ; poly = [] ; tc = [] ; row = [] }
  in
  let constraints_a = get_constraints variable_repr_a in
  let constraints_b = get_constraints variable_repr_b in
  let all_constraints = {
      constructor = constraints_a.constructor @ constraints_b.constructor ;
      poly        = constraints_a.poly        @ constraints_b.poly        ;
      tc          = constraints_a.tc          @ constraints_b.tc          ;
      row         = constraints_a.row         @ constraints_b.row         ;
    } in
  let grouped_by_variable =
    Map.add variable_repr_a all_constraints dbs.grouped_by_variable in
  let grouped_by_variable =
    Map.remove variable_repr_b grouped_by_variable in
  let dbs = { dbs with grouped_by_variable} in
  dbs
