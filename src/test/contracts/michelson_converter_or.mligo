type st4 =
  | Foo4 of int
  | Bar4 of nat
  | Baz4 of string
  | Boz4 of bool

type st3 =
  | Foo3 of int
  | Bar3 of nat
  | Baz3 of string

(** convert_to **)

let vst3 = Bar3 3n
let vst4 = Baz4 "eq"

let str3 = Layout.convert_to_right_comb (vst3:st3)
let str4 = Layout.convert_to_right_comb (vst4:st4)

let stl3 = Layout.convert_to_left_comb (vst3:st3)
let stl4 = Layout.convert_to_left_comb (vst4:st4)