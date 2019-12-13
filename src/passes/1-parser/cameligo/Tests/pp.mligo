type q = {a: int; b: {c: string}}
type r = int list
type s = (int, address) map
type t = int
type u = {a: int; b: t * char}
type v = int * (string * address)
type w = timestamp * nat -> (string, address) map
type x = A | B of t * int | C of int -> (string -> int)

let x = 4
let y : t = (if true then -3 + f x x else 0) - 1
let f (x: int) y = (x : int)
let z : (t) = y
let w =
  match f 3 with
    None -> []
  | Some (1::[2;3]) -> [4;5]::[]
let n : nat = 0n
let a = A
let b = B a
let c = C (a, B (a))
let d = None
let e = Some (a, B b)
let z = z.1.2
let v = "hello" ^ "world" ^ "!"
let w = Map.literal [(1,"1"); (2,"2")]
