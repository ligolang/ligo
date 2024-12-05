[@@@ligo]

type data =
  | A
  | B
  | C

type storage = data
type return = operation list * storage

let x = 1

let[@entry] next () storage : return =
  let storage =
    match storage with
    | A -> B
    | B -> C
    | C -> A
  in
  [], storage
