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

let match_bool (b: bool) : int =
  match b with
    true -> 10
  | false -> 0

let match_list (l: int list) : int =
  match l with
    hd :: tl -> hd
  | [] -> 10

let match_option (i : int option) : int =
  match i with
    Some n -> n
  | None -> 0
