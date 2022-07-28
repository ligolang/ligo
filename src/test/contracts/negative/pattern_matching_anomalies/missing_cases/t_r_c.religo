type p = One | Two | Three | Four;
type r = { a : int, b : p, c : nat };

type t = (int, (r, string))

let s = (x : t) => 
  switch (x) {
  | (n, ({ a, c, b: One }, s))  => ()
  | (n, ({ a, b: Four, c }, s)) => ()
  }