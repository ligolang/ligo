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

(*convert from*)

// let s = "eq"
// let test_input_pair_r = (1,(2n,(s,true)))
// let test_input_pair_l = (((1,2n), s), true)

type param_r = st4 michelson_or_right_comb
let main_r (p, s : param_r * string) : (operation list * string) =
  // let r4 : t4 = Layout.convert_from_right_comb p in
  ([] : operation list),  "hey"

type param_l = st4 michelson_or_left_comb
let main_l (p, s : param_l * string) : (operation list * string) =
  // let r4 : t4 = Layout.convert_from_left_comb p in
  ([] : operation list),  "hey"