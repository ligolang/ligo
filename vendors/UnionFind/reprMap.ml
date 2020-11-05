(* Map where the key is the repr *)

type ('item, 'a) t = {
  merge: 'a -> 'a -> 'a;
  map: ('item, 'a) PolyMap.t
}

let create ~cmp ~merge = {
  merge;
  map = PolyMap.create ~cmp;
}

let alias ~demoted_repr ~new_repr (m : (_,_) t) =
  { m with
    map = match PolyMap.find_opt demoted_repr m.map with
      | None -> m.map
      | Some other_value ->
        PolyMap.update
          new_repr
          (function
              None -> None
            | Some v -> Some (m.merge other_value v))
          m.map }

(* We don't export empty, since elements can be removed via
   List.fold add (empty s) (List.filter … @@ elements s) *)
(* let empty m = { m with map = PolyMap.empty m.map } *)

let is_empty m = PolyMap.is_empty m.map

let add k v m = { m with map = PolyMap.add k v m.map }

let add_opt k v m = match PolyMap.find_opt k m.map with Some _ -> None | None -> Some (add k v m)

let monotonic_update k f m = {
  m with map = PolyMap.update k (function None -> Some (f None) | Some v -> Some (f (Some v))) m.map }

(* No removal, should be monotonic aside from merges due to aliasing *)
(* let remove k m = { m with map = PolyMap.remove k m.map } *)

(* find throws an exception, disabling it *)
(* let find k m = PolyMap.find k m.map *)

let find_opt k m = PolyMap.find_opt k m.map

let find_default k default m =
  let value, map = PolyMap.find_default k default m.map in
  value, { m with map }

let has_key k m = PolyMap.has_key k m.map

let bindings m = PolyMap.bindings m.map
