open Ast_typed.Types
open UnionFind
open Trace

(* Haskell doesn't have easy-to-use type-level functions or types as
   fields of records, so we're bending its syntax here.

   type "Assignments.t" typeVariable = map typeVariable
   c_constructor_simpl data Assignments :: Plugin "Assignments.t" *)

type 'typeVariable t = ('typeVariable, c_constructor_simpl) ReprMap.t
let create_state ~cmp =
  let merge c1 c2 = assert (Ast_typed.Compare.c_constructor_simpl c1 c2 = 0); c1 in
  ReprMap.create ~cmp ~merge

(** Stores the first assignment ('a = ctor('b, …)) that is encountered
    (all assignments should use compatible types).

    Subsequent ('a = ctor('b2, …)) with the same 'a are ignored. *)
let add_constraint repr state new_constraint =
  match new_constraint with
  | SC_Constructor ({tv ; c_tag = _ ; tv_list = _} as c) ->
    Option.unopt ~default:state @@ ReprMap.add_opt (repr tv) c state
  | _ -> state

let remove_constraint _repr state _constraint_to_remove =
  (* assignments cannot be remove (they are similar to instanciations
     of existential variables in Coq, and happen globally regardless
     of the constraints available in the database). *)
  ok state

let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun merge_keys state -> merge_keys.map state

let find_opt : 'type_variable -> 'type_variable t -> c_constructor_simpl option = ReprMap.find_opt
