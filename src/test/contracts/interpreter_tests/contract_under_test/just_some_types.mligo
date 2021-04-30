type some_r = [@layout:comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit }

type some_v = [@layout:comb] | Foo of int | Bar of string | Bare of string | Baz of nat

let f = fun (x:some_r) -> x.one