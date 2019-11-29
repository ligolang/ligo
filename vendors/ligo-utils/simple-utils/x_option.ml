(* Constructors *)
let none = None
let some x = Some x
let return = some

(* Destructors *)
let unopt ~default x = match x with
  | None -> default
  | Some x -> x
let unopt_exn x = match x with
  | None -> raise Not_found
  | Some x -> x

(* Base Tranformers *)
let bind f = function
  | None -> None
  | Some x -> f x
let map f x =
  let f' y = return @@ f y in
  bind f' x

(* Syntax *)
let (>>=) x f = bind f x

(* Interaction with List *)
let to_list = function
  | None -> []
  | Some x -> [ x ]
let collapse_list = fun l ->
  List.concat @@ List.map to_list l

(* Combinators *)
let bind_eager_or = fun a b -> match (a , b) with
  | Some a , _ -> Some a
  | _ , Some b -> Some b
  | _ -> None

let bind_union (a , b) = match (a , b) with
  | Some x , _ -> Some (`Left x)
  | None , Some x -> Some (`Right x)
  | _ -> None

let rec bind_list = fun lst ->
  (* TODO: recursive terminal *)
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

let equal eq x y =
  match (x, y) with
  | (None, None) -> true
  | (Some x, Some y) -> eq x y
  | _ -> false

let compare compare x y =
  match (x, y) with
  | (None, None) -> 0
  | (None, Some _) -> -1
  | (Some _, None) -> 1
  | (Some x, Some y) -> compare x y
