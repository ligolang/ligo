open Core

type 'a t = 'a Nonempty_list.t = (::) of 'a * 'a list [@@deriving eq, compare, yojson, hash, sexp, fold, map, iter]

let make x l = x :: l

let sexp_of_t f Nonempty_list.(hd::tl) = List.sexp_of_t f (hd :: tl)

let t_of_sexp f (x : Sexp.t) : 'a t =
  match x with
  | Atom _ -> [f x]
  | List (x::l) -> f x :: List.map ~f l
  | List [] -> assert false

let of_list_opt = function
  | [] -> None
  | x :: l -> Some Nonempty_list.(x :: l)

let append Nonempty_list.(x::l) Nonempty_list.(x'::l') =
  Nonempty_list.(x :: List.(append l (x' :: l')))

let collect : 'a option Nonempty_list.t -> 'a Nonempty_list.t option = function
  | None :: _ -> None
  | Some x :: l ->
    match Option.all l with
    | None -> None
    | Some l -> Some (x :: l)

let rec fold_right1 ~f : 'a Nonempty_list.t -> 'a = function
| [hd] -> hd
| hd :: (x::l) -> f hd @@ fold_right1 ~f Nonempty_list.(x::l)

type json = Yojson.Safe.t

let yojson_of_t f s = `List Nonempty_list.(to_list @@ map ~f s)
