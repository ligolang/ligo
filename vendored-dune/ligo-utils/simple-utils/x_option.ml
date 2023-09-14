open Core
include Option

(* Syntax *)
let ( let* ) x f = bind ~f x

(* Combinators *)
let bind_eager_or a b =
  match a, b with
  | Some a, _ -> Some a
  | _, Some b -> Some b
  | _ -> None

let map_pair_or (fa, fb) p = bind_eager_or (fa p) (fb p)

let bind_union (a, b) =
  match a, b with
  | Some x, _ -> Some (`Left x)
  | None, Some x -> Some (`Right x)
  | _ -> None

let bind_pair (a, b) =
  let* a in
  let* b in
  Some (a, b)

let unzip = function
  | Some (a, b) -> Some a, Some b
  | None -> None, None

let bind_map_pair f (a, b) = bind_pair (f a, f b)

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
 fun f acc opt ->
  match opt with
  | Some x ->
    let acc, x = f acc x in
    acc, Some x
  | None -> acc, None
