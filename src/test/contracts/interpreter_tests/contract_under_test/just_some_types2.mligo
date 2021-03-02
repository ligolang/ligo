type some_r = [@layout:tree] { one : int ; two : nat ; three : string ; four : bytes ; five : unit }

type some_v = [@layout:tree] | Foo of int | Bar of string | Bare of string | Baz of nat

let f = fun (x:some_r) -> x.one