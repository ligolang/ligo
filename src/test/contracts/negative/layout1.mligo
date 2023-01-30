type r1 = [@layout:comb] {foo : string; bar : string}
type r2 = {bar : string; foo : string}

let main (_ : unit) (_ : r1) : operation list * r1 =
  ([], ({bar = "bar"; foo = "foo"} : r2))
