type p = One | Two | Three | Four;
type r = ((string, int), (p, nat));

type t = { a : int, b : r, c : nat }

let s = (x : t) => 
  switch (x) {
  | { a, c, b: ((x, y), (One, z)) } => ()
  }