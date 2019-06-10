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
