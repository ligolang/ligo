[@@@ligo]

type data =
  | A
  | B
  | C

type storage = int * data
type return = operation list * storage

(* let[@entry] set new_storage (_storage : storage) : return = [], new_storage *)

let[@entry] next () storage : return =
  let n, data = storage in
  let data =
    match data with
    | A -> B
    | B -> C
    | C -> A
  in
  let storage = n, data in
  [], storage
