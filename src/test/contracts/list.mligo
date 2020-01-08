type storage = int * int list

type param = int list

let x : int list = []
let y : int list = [3; 4; 5]
let z : int list = 2::y

let main (p, s: param * storage) =
  let storage =
    match p with
          [] -> s
    | hd::tl -> s.0 + hd, tl
  in ([] : operation list), storage

let fold_op (s: int list) : int =
  let aggregate = fun (t: int * int) -> t.0 + t.1
  in List.fold aggregate s 10

let map_op (s: int list) : int list =
  List.map (fun (cur: int) -> cur + 1) s

let iter_op (s : int list) : unit =
  let do_nothing = fun (_: int) -> unit
  in List.iter do_nothing s
