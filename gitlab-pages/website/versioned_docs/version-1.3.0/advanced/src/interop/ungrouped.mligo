type animal =
[@layout comb]
| Elephant
| Dog
| Cat
type artist =
  [@layout comb] {
  genre : string;
  since : timestamp;
  name  : string
}
type w_and_v = (int, "w", nat, "v") michelson_pair
type x_and = (string, "x", w_and_v, "other") michelson_pair
type y_or = (unit, "y", x_and, "other") michelson_or
type z_or = (unit, "z", y_or, "other") michelson_or
let z : z_or = M_left unit

let y_1 : y_or = M_left unit
let y   : z_or = M_right y_1

let x_pair = "foo", (2, 3n)
let x_1 : y_or = M_right x_pair
let x : z_or = M_right y_1
type storage = int

type parameter =
 | Left of int
 | Right of int

[@entry]
let main (p : parameter) (x : storage): (operation list * storage) =
  [],
  (match p with
  | Left i -> x - i
  | Right i -> x + i
  )