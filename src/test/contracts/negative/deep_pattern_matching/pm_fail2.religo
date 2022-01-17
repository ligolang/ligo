type myt = Nil(unit) | Cons ((int, int))

let t = (x: (myt, myt)) =>
  switch(x) {
  | (Nil , (a,b,c)) => 1
  | (xs  , Nil) => 2
  | (Cons ((a,b)) , Cons ((c,d))) => a + b + c + d
  }