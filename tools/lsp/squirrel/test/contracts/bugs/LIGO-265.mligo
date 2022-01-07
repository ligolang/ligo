let funny (a : (Foo | Bar) list) : int = 1

let lol (a : int) : int = funny [Bar]

type x =
  | A
  | B of int
  | C of (X of int | Y of int)
