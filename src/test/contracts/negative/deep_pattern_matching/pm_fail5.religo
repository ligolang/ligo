type foo = None_fake(unit) | Some_fake(int)

let t = (x: option(int)) =>
  switch(x) {
  | Some_fake(x) => x
  | None_fake    => 1
  }