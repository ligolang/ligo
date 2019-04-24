include Tezos_stdlib.Option

let lr (a , b) = match (a , b) with
  | Some x , _ -> Some (`Left x)
  | None , Some x -> Some (`Right x)
  | _ -> None

(* TODO: recursive terminal *)
let rec bind_list = fun lst ->
  match lst with
  | [] -> Some []
  | hd :: tl -> (
      match hd with
      | None -> None
      | Some hd' -> (
          match bind_list tl with
          | None -> None
          | Some tl' -> Some (hd' :: tl')
        )
    )

let bind_pair = fun (a , b) ->
  a >>= fun a' ->
  b >>= fun b' ->
  Some (a' , b')

let bind_map_list = fun f lst -> bind_list (X_list.map f lst)

let bind_map_pair = fun f (a , b) -> bind_pair (f a , f b)

let bind_smap (s:_ X_map.String.t) =
  let open X_map.String in
  let aux k v prev =
    prev >>= fun prev' ->
    v >>= fun v' ->
    Some (add k v' prev') in
  fold aux s (Some empty)

let bind_map_smap f smap = bind_smap (X_map.String.map f smap)
