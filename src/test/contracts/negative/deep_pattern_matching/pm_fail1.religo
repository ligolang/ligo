type myt = Nil(unit) | Cons ((int, int));
type myr = { a : int , b : nat , c : string }

let t = (x: (myt, myt)) =>
  switch(x) {
  | (Nil , {a : a , b : b , c : c}) => 1
  | (xs  , Nil) => 2
  | (Cons ((a,b)) , Cons ((c,d))) => a + b + c + d
  }