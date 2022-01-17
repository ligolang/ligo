type myd = One(int) | Two(unit)

let t = (x : myd) =>
  switch(x) {
  | One(a) => 2
  | Two    => a
  }