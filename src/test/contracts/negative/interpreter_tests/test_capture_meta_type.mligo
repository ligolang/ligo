
module C = struct
  [@entry] let main (_ : unit) (_ : unit) : operation list * unit = [], ()
end

type t = { x : int ; y : (C parameter_of, unit) typed_address }

let orig =
  Test.originate (contract_of C) () 0tez

let v : t = { x = 42 ; y = orig.addr }

let w = v.x

let f = fun (_ : unit) -> v.x

let g = fun (_ : unit) -> f ()

let test = Test.eval g
