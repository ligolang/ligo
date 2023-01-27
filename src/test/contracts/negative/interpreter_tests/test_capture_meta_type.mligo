type t = { x : int ; y : (unit, unit) typed_address }

let main ((_, _) : unit * unit) : operation list * unit = [], ()

let ta, _, _ =
  Test.originate_uncurried main () 0tez

let v = { x = 42 ; y = ta }

let w = v.x

let f = fun (_ : unit) -> v.x

let g = fun (_ : unit) -> f ()

let test = Test.eval g
