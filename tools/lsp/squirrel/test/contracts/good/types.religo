type test =
  | Alt1(int => string)
  | Alt2
  | Alt3({field1: int, field2: ((int, int) => (Join(int) | Empty))});

let main = (((p, _): (unit, unit)): (list(operation), unit)) =>
  (([] : list(operation)), p)
