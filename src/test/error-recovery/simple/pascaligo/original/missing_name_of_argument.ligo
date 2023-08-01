type storage is int

type parameter is list (int)

type s is
  record [
    a: int;
    b: bool;
    c: string;
    d: list (list (list (int)));
  ]

const s_a : s = 
  record [ a = 42; b = false ]

const s_b : s =
  s_a with record [ a = 32 ]

type return is list (operation) * storage

function hd (const x : list (int)) : int is
  case x of [
   | nil    -> -1
   | x # xs -> x
  ]

function main (const a: parameter; const b: storage) : return is
  ((nil : list (operation)), (hd (a) + (b + 8) * 11))
