(* Added functions to Core.Option *)

open Core

(* Syntax *)

let ( let* ) x f = Option.bind ~f x

(* Combinators *)

let map_pair_or (f, g) p =
  match f p, g p with
  | Some a, _ -> Some a
  | _, Some b -> Some b
  | _ -> None

let unzip = function
  | Some (a, b) -> Some a, Some b
  | None -> None, None

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a option -> 'acc * 'b option =
 fun f acc -> function
   | Some x -> let acc, x = f acc x in acc, Some x
   | None -> acc, None

let nonempty_all (ne : 'a option Nonempty_list.t) : 'a Nonempty_list.t option =
  match ne with
  | None :: _ -> None
  | Some hd :: tl ->
    match Option.all tl with
    | None -> None
    | Some tl -> Some (hd :: tl)
