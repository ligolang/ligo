type inner =
  { inner_field1 : int
  ; inner_field2 : int
  }

type nested_type =
  { simple_field : string
  ; complex_field : inner
  }

let example =
  let nested = { inner_field1 = 0 ; inner_field2 = 1 } in
  { simple_field = "123" ; complex_field = nested }

[@entry]
let main () (s : int) : operation list * int =
  let a = example.complex_field.inner_field1 in
  (([] : operation list), s + a)
