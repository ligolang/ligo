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
