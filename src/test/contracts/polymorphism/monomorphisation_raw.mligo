let foo : bytes = [%michelson ({| { PACK } |} (fun () -> (failwith "haha" : int)) : bytes)]
let foo2 : bytes = [%michelson ({| { AND } |} (failwith "hoho" : bytes) (failwith "hehe" : bytes) : bytes)]
let id (type a) (x : a) : a = x
let foo3 : bytes = [%michelson ({| { LSR } |} (id (0x12 : bytes)) (id (1n)) : bytes)]

module C = struct
  [@entry] let doer (() : unit) (_ : bytes): operation list * bytes = [], foo
end
