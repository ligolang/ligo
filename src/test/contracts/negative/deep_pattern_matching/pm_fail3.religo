type myt = Nil(unit) | Cons ((int, int))

let t = (x: (myt, (int , int , int))) =>
  switch(x) {
  | (xs , (a,b,c)) => 1
  | (xs , (c,b,a)) => 2
  }