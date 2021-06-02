open Types

let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_lmapi f map = bind_lmap (LMap.mapi f map)
let bind_fold_lmap f init lmap =
  let open Trace in
  let open LMap  in
  let aux k v acc =
    acc >>? fun acc ->
    f acc k v in
  ok init |> fold aux lmap
let bind_iter_lmap f lmap =
  let open Trace in
  let open LMap  in
  let aux k v unit =
    unit >>? fun () ->
    f k v in
  ok () |> fold aux lmap


let bind_fold_map_lmap f init lmap =
  let open Trace in
  let open LMap  in
  let aux k v acc =
    acc >>? fun (acc',prev') ->
    let* (acc',v') = f acc' k v in
    let prev' = add k v' prev' in
    ok @@ (acc', prev') in
  ok (init, empty) |> fold aux lmap

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some e1, Some e2 -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f:(fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    List.map ~f:snd @@ tuple_of_record m
  else
    LMap.to_list m

let kv_list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    tuple_of_record m
  else
    LMap.to_kv_list m

let rec fold_pattern : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern -> 'a =
  fun f acc p ->
    let acc' = f acc p in
    match p.wrap_content with
    | P_unit -> acc'
    | P_var _ -> acc'
    | P_list lp -> (
      match lp with
      | Cons (pa,pb) -> fold_pattern f (fold_pattern f acc' pb) pa
      | List lp -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp 
    )
    | P_variant (_,p) -> fold_pattern f acc' p
    | P_tuple lp -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp
    | P_record (_,lp) -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp

open Trace
let fold_pattern_list f acc l = List.fold_left ~f:(fold_pattern f) ~init:acc l

let rec map_pattern_t : ('a binder -> ('b binder, 'err) result) -> 'a pattern -> ('b pattern, 'err) result =
  fun f p ->
    let self = map_pattern_t f in
    let ret wrap_content = ok { p with wrap_content } in
    match p.wrap_content with
    | P_unit -> ret P_unit
    | P_var b ->
      let* b' = f b in
      ret (P_var b')
    | P_list lp -> (
      let* lp =
        match lp with
        | Cons (pa,pb) ->
          let* pa = self pa in
          let* pb = self pb in
          ok @@ (Cons (pa, pb) : 'b list_pattern)
        | List lp ->
          let* lp = bind_map_list self lp in
          ok @@ (List lp : 'b list_pattern)
      in
      ret @@ P_list lp
    )
    | P_variant (l,p) -> (
      let* p = self p in
      ret @@ P_variant (l,p)
    )
    | P_tuple lp ->
      let* lp = bind_map_list self lp in
      ret @@ P_tuple lp
    | P_record (x,lp) ->
      let* lp = bind_map_list self lp in
      ret @@ P_record (x,lp)

let var_attribute = { const_or_var = Some `Var }
let const_attribute = { const_or_var = Some `Const }
let empty_attribute = { const_or_var = None }
