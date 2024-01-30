open Core
include List

let rec remove n = function
  | [] -> raise (Failure "List.remove")
  | _ :: tl when n = 0 -> tl
  | hd :: tl -> hd :: remove (n - 1) tl

let set_nth new_i l new_v =
  mapi ~f:(fun old_i old_v -> if old_i = new_i then new_v else old_v) l

let rec remove_element ?compare:cmp x lst =
  let compare = Option.value ~default:Caml.compare cmp in
  match lst with
  | [] -> raise (Failure "List.remove_element")
  | hd :: tl when compare x hd = 0 -> tl
  | hd :: tl -> hd :: remove_element ~compare x tl

let repeat n x = init ~f:(fun _ -> x) n

let to_pair = function
  | [ a; b ] -> Some (a, b)
  | _ -> None

let to_singleton = function
  | [ a ] -> Some a
  | _ -> None

let fold_map2_exn ~f ~init a b =
  fold_map ~f:(fun init (a, b) -> f init a b) ~init @@ zip_exn a b

let rec fold_map_right ~f ~init = function
  | [] -> init, []
  | hd :: tl ->
    let init, tl = fold_map_right ~f ~init tl in
    let init, hd = f init hd in
    init, hd :: tl

let uncons = function
  | [] -> None
  | hd :: tl -> Some (hd, tl)

let repeat x n =
  let rec aux n xs = if n <= 0 then xs else aux (n - 1) (x :: xs) in
  aux n []

let zip_opt a b =
  match List.zip a b with
  | Or_unequal_lengths.Ok x -> Some x
  | Or_unequal_lengths.Unequal_lengths -> None

let rec deoptionalize xs =
  match xs with
  | [] -> Some []
  | None :: _ -> None
  | Some x :: xs ->
    let xs = deoptionalize xs in
    (match xs with
    | None -> None
    | Some xs -> Some (x :: xs))

let rec drop_common_prefix ~(equal : 'a -> 'b -> bool) ~(prefix : 'a t) (list : 'b t)
    : 'b t
  =
  match prefix, list with
  | x :: xs, y :: ys when equal x y -> drop_common_prefix ~equal ~prefix:xs ys
  | _, _ -> list

module Ne = struct
  type 'a t = 'a * 'a list [@@deriving eq, compare, yojson, hash, sexp, fold]

  let sexp_of_t (f : 'a -> Sexp.t) ((hd, tl) : 'a t) : Sexp.t = List.sexp_of_t f (hd :: tl)

  let t_of_sexp : type a. (Sexp.t -> a) -> Sexp.t -> a t =
   fun f x ->
    match x with
    | Atom _ -> f x, []
    | List lst ->
      let lst = List.map ~f lst in
      List.hd_exn lst, List.tl_exn lst

  let unzip ((hd, tl) : _ t) =
    let a, b = hd
    and la, lb = unzip tl in
    (a, la), (b, lb)

  let of_list lst = List.hd_exn lst, List.tl_exn lst (* TODO: Remove *)

  let of_list_opt = function
    | [] -> None
    | x :: xs -> Some (x, xs)

  let to_list ((hd, tl) : _ t) = hd :: tl
  let singleton hd : 'a t = hd, []

  let last ((hd, tl) : _ t) =
    match tl with
    | [] -> hd
    | _ -> List.last_exn tl

  let hd : 'a t -> 'a = fst
  let cons : 'a -> 'a t -> 'a t = fun hd' (hd, tl) -> hd', hd :: tl

  let iter f ((hd, tl) : _ t) =
    f hd;
    List.iter ~f tl

  let map f ((hd, tl) : _ t) = f hd, List.map ~f tl
  let fold_left ~f ~init ((hd, tl) : _ t) = List.fold_left ~f ~init:(f init hd) tl

  let rec fold_right1 ~f ((hd, tl) : _ t) =
    match tl with
    | [] -> hd
    | hdtl :: tltl -> f hd @@ fold_right1 ~f (hdtl, tltl)

  let fold_right ~f ~init ((hd, tl) : _ t) = f hd (List.fold_right ~f ~init tl)

  let fold_map ~f ~init ((hd, tl) : _ t) =
    let init, hd = f init hd in
    let init, tl = List.fold_map ~f ~init tl in
    init, (hd, tl)

  let hd_map : _ -> 'a t -> 'a t = fun f (hd, tl) -> f hd, tl

  let mapi f ((hd, tl) : _ t) =
    let lst = List.mapi ~f (hd :: tl) in
    of_list lst

  let concat ((hd, tl) : _ t) = hd @ List.concat tl
  let rev (lst : _ t) = of_list @@ List.rev @@ to_list lst

  let find_map f ((hd, tl) : _ t) =
    match f hd with
    | Some x -> Some x
    | None -> find_map ~f tl

  let append : 'a t -> 'a t -> 'a t =
   fun (hd, tl) (hd', tl') -> hd, List.append tl @@ (hd' :: tl')

  let length (_, tl) = 1 + List.length tl

  (* [head_permute] [lst] : if lst == [A ; B ; C] returns [A ; B ; C] [B ; C ; A] [C ; A ; B] *)
  let head_permute : 'a t -> 'a t t =
   fun lst ->
    let rec aux (acc : 'a t t) (lst : 'a t) =
      if length acc = length lst
      then acc
      else (
        let hd, tl = lst in
        (* match lst with *)
        (* | [] -> raise (Failure "List.head_permute") *)
        (* | hd::tl -> *)
        let p = append (of_list tl) (singleton hd) in
        aux (cons p acc) p)
    in
    rev @@ aux (singleton lst) lst

  let deoptionalize : 'a option t -> 'a t option =
   fun lst ->
    let hd, tl = lst in
    match hd with
    | None -> None
    | Some hd ->
      let tl = deoptionalize tl in
      (match tl with
      | None -> None
      | Some tl -> Some (hd, tl))
end
