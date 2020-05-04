type st4 =
  | Foo4 of int
  | Bar4 of nat
  | Baz4 of string
  | Boz4 of bool

type st3 =
  | Foo3 of int
  | Bar3 of nat
  | Baz3 of string

(*convert from*)

type tr3 = (string,"baz4",bool,"boz4")michelson_or
type tr2 = (nat,"bar4",tr3,"") michelson_or
type tr1 = (int,"foo4",tr2,"")michelson_or
let vr : tr1 = M_right (M_right (M_left "eq":tr3):tr2)

type tl3 = (int,"foo4",nat,"bar4")michelson_or
type tl2 = (tl3,"",string,"baz4") michelson_or
type tl1 = (tl2,"",bool,"boz4")michelson_or
let vl : tl1 = M_left (M_right "eq":tl2)

type param_r = st4 michelson_or_right_comb
let main_r (p, s : param_r * st4) : (operation list * st4) =
  let r4 : st4 = Layout.convert_from_right_comb p in
  ([] : operation list), r4

type param_l = st4 michelson_or_left_comb
let main_l (p, s : param_l * st4) : (operation list * st4) =
  let r4 : st4 = Layout.convert_from_left_comb p in
  ([] : operation list), r4

(** convert_to **)

let vst3 = Bar3 3n
let vst4 = Baz4 "eq"

let str3 = Layout.convert_to_right_comb (vst3:st3)
let str4 = Layout.convert_to_right_comb (vst4:st4)

let stl3 = Layout.convert_to_left_comb (vst3:st3)
let stl4 = Layout.convert_to_left_comb (vst4:st4)
