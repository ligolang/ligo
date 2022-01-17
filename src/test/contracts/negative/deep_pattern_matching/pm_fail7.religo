type foo = A(unit) | B(unit)

let t = (x: foo) =>
  switch(x) {
  | A => "hey"
  | B => 2
  }