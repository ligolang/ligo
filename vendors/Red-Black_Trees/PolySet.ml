(* Polymorphic sets *)

module RB = RedBlack

type 'elt t = {
  tree : 'elt RB.t;
  cmp  : 'elt -> 'elt -> int
}

type 'elt set = 'elt t

let create ~cmp = {tree = RB.empty; cmp}

let empty set = {tree = RB.empty; cmp=set.cmp}

let is_empty set = RB.is_empty set.tree

let add elt set = {set with tree = RB.add ~cmp:set.cmp RB.New elt set.tree}

let find elt set =
  try RB.find ~cmp:set.cmp elt set.tree with
    Not_found -> raise Not_found

let find_opt elt set = RB.find_opt ~cmp:set.cmp elt set.tree

let elements set = RB.elements set.tree

let iter f set = RB.iter f set.tree

let fold_inc f set = RB.fold_inc (fun ~elt -> f elt) set.tree
