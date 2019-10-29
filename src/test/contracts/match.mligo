type storage = int

type param =
  Add of int
| Sub of int

let%entry main (p : param) storage =
  let storage =
    storage +
      (match p with
         Add n -> n
       | Sub n -> 0-n)
  in (([] : operation list), storage)

let match_list (l: int list) : int =
  match l with
    hd :: tl -> hd
  | [] -> 10

(* TODO: Add support for matching options

type option_param =
  Add of int option
| Sub of int option

let match_option (p : option_param) storage =
  let storage =
    storage +
      (match p with
        Some (Add n) -> n
      | Some (Sub n) -> 0 - n
      | None -> 0)
  in (([] : operation list) , storage)

*)
