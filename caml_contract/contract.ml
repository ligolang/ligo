[@@@ligo]

type data =
  | A
  | B
  | C

type storage = data
type return = operation option * storage

let[@entry] next () storage : return =
  let storage =
    match storage with
    | A -> B
    | B -> C
    | C -> A
  in
  None, storage
