open Types

let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_cmap (c:_ constructor_map) =
  let open Trace in
  let open CMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux c (ok empty)

let bind_fold_lmap f init (lmap:_ LMap.t) =
  let open Trace in
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  LMap.fold aux lmap init

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_cmap f map = bind_cmap (CMap.map f map)

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map (fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all (fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  let open Trace in
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some e1, Some e2 -> ok (e1,e2)
  | _ -> simple_fail "not a pair"
