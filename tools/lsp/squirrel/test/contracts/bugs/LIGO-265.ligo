function funny (const a : list (Foo | Bar)) : int is 1

function lol (const a : int) : int is funny (list [Bar])

type x is
  | A
  | B of int
  | C of (X of int | Y of int)
