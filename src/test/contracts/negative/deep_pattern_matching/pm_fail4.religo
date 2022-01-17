type myt = Nil(unit) | Cons ((int, int))

let t = (x: (myt, myt)) =>
  switch(x) {
  | (Nil , ys)  => 1
  | (xs  , Nil) => 2
  }