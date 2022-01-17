
type parameter =
  Increment((int, int, int))
| Reset(unit)

let main = (action : parameter) : int =>
  switch(action) {
  | Increment((n, m)) => 0
  | Reset             => 0
  }