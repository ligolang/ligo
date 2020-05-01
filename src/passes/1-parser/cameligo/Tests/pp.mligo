type r = int list
type t = int
type s = (int,address) map
type w = timestamp * nat -> (string, address) map -> t
type v = int * (string * address)
type u = {a: int; b: t * char}
type q = {a: int; b: {c: string}}
type x = A | B of t * int | C of int -> (string -> int)
type y = "foo"
let x (_, (y: char)) = 4
let y {x=(_,y); z=3} = x
let z : (t) = y
let w =
  match f 3 with
    None -> []
  | Some (1::[2;3]) -> [4;5]::[]
let y : t = (if true then -3 + f x x else 0) - 1
let f (x: int) y = (x : int)
let n : nat = 0n
let a = A
let b = B a
let c = C (a, B (a))
let d = None
let e = Some (a, B b)
let z = z.1.2
let v = "hello" ^ "world" ^ "!"
let w = Map.literal [(1,"1"); (2,"2")]

let r = { field = 0}
let r = { r with field = 42}
