[@@@ligo]

type data =
  | A
  | B
  | C

type storage = data
type return = operation option * storage

external add : int -> int -> int = "%ligo" [@@ligo.internal.constant "ADD"]

let[@entry] next () storage : return =
  let storage =
    match storage with
    | A -> B
    | B -> C
    | C -> A
  in
  None, storage

let x = "OCaml in Ligo"
let x = "x.ligo.ml"
let y = "TypeScript in Ligo"
let y = "x.ligo.ts"
