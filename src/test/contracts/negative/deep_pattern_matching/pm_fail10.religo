type myd = One(int) | Two(unit)

let t = (x : myd) =>
  switch(x) {
  | One(1) => 2
  | Two    => 1
  }
