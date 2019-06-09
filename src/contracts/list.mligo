type storage = int * int list

type param = int list

let%entry main (p : param) storage =
  let storage =
    match p with
      [] -> storage
    | hd::tl -> storage.(0) + hd, tl
  in (([] : operation list), storage)
