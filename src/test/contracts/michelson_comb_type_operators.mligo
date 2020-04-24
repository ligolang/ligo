type t3 = { foo : int ; bar : nat ;  baz : string}

type param_r = t3 michelson_right_comb
type param_l = t3 michelson_left_comb

let main_r (action, store : param_r * unit) : (operation list * unit) =
 ([] : operation list),  unit

let main_l (action, store : param_l * unit) : (operation list * unit) =
 ([] : operation list),  unit