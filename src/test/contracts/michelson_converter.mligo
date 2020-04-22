type t3 = { foo : int ; bar : nat ;  baz : string}
let v3 = { foo = 2 ; bar = 3n ; baz = "q" }

type t4 = { one: int ; two : nat ; three : string ; four : bool}
let v4 = { one = 2 ; two = 3n ; three = "q" ; four = true }

let r3 = Layout.convert_to_right_comb (v3:t3)
let r4 = Layout.convert_to_right_comb (v4:t4)

let l3 = Layout.convert_to_left_comb (v3:t3)
let l4 = Layout.convert_to_left_comb (v4:t4)