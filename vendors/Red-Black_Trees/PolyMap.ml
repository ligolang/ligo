(* Polymorphic maps *)

module RB = RedBlack

type ('key, 'value) t = {
  tree : ('key * 'value) RB.t;
  cmp  : 'key -> 'key -> int
}

type ('key, 'value) map = ('key, 'value) t

let create ~cmp = {tree = RB.empty; cmp}

let empty = {tree = RB.empty; cmp=Pervasives.compare}

let is_empty map = RB.is_empty map.tree

let add key value map =
  let cmp (k1,_) (k2,_) = map.cmp k1 k2 in
  {map with tree = RB.add ~cmp RB.New (key, value) map.tree}

exception Not_found

let find key map =
  let cmp k1 (k2,_) = map.cmp k1 k2 in
  try snd (RB.find ~cmp key map.tree) with
    RB.Not_found -> raise Not_found

let find_opt key map =
  try Some (find key map) with Not_found -> None

let bindings map =
  RB.fold_dec (fun ~elt ~acc -> elt::acc) ~init:[] map.tree

let iter f map = RB.iter (fun (k,v) -> f k v) map.tree

let fold_inc f map = RB.fold_inc (fun ~elt:(k,v) -> f k v) map.tree
