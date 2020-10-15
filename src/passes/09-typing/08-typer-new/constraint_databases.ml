module Map = RedBlackTrees.PolyMap
module UF = UnionFind.Poly2
open Ast_typed.Types
open Trace

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

exception CouldNotRemove
exception NestedFailure of string

let rm_constraints_related_to : type_variable -> constraints -> structured_dbs -> (structured_dbs, _) result =
  fun variable c dbs ->
    let variable_repr , aliases = UF.get_or_set variable dbs.aliases in
    let dbs = { dbs with aliases } in
    (* TODO: remove the empty set if a variable is not associated with
       any constraint after this removal. *)
    let rm_typeclass_simpl : c_typeclass_simpl list -> c_typeclass_simpl list -> c_typeclass_simpl list =
      fun x c ->
        (* TODO: use a set, not a list. *)
        List.fold_left (fun x' ci ->
            try
              List.remove_element
                ~compare:(fun a b ->
                    try
                      Ast_typed.Compare.constraint_identifier a.id_typeclass_simpl b.id_typeclass_simpl
                    with
                      Failure msg -> raise (NestedFailure msg))
                ci
                x'
            with
              Failure _msg -> raise CouldNotRemove
            | NestedFailure msg -> raise (Failure msg))
          x
          c in
    let%bind grouped_by_variable =
      match
        Map.update variable_repr (function
              None -> raise CouldNotRemove (* Some c *)
            | Some (x : constraints) -> Some {
                constructor = (assert (List.length c.constructor = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.constructor) ;
                poly        = (assert (List.length c.poly        = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.poly       ) ;
                tc          = rm_typeclass_simpl x.tc c.tc         ;
                row         = (assert (List.length c.row         = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.row        ) ;
              })
          dbs.grouped_by_variable
      with
        exception CouldNotRemove -> fail Typer_common.Errors.could_not_remove
      | result -> ok result
    in
    let dbs = { dbs with grouped_by_variable } in
    ok dbs

let register_by_constraint_identifier : c_typeclass_simpl -> structured_dbs -> structured_dbs =
  fun c dbs ->
  { dbs with by_constraint_identifier = Map.add c.id_typeclass_simpl c dbs.by_constraint_identifier }

open Heuristic_tc_fundep_utils

let register_typeclasses_constrained_by : c_typeclass_simpl -> structured_dbs -> structured_dbs =
  fun c dbs ->
  let tc = tc_to_constraint_identifier c in
  let aux' = function
      Some set -> Some (Set.add tc set)
    | None -> Some (Set.add tc (Set.create ~cmp:Ast_typed.Compare.constraint_identifier)) in
  let aux typeclasses_constrained_by tv =
    Map.update tv aux' typeclasses_constrained_by in
  let typeclasses_constrained_by =
    List.fold_left
      aux
      dbs.typeclasses_constrained_by
      (List.rev (constraint_identifier_to_tc dbs tc).args) in
  { dbs with typeclasses_constrained_by }


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
