type storage = {fn : (int -> int) option; value : int}
type result = operation list * storage

let call (fn : (int -> int) option) (value : int) =
  match fn with
    Some f -> f value
  | None -> failwith "Lambda is not set"

[@entry]
let setFunction (fn : int -> int) (s : storage) : result =
  [], { s with fn = Some fn }

[@entry]
let callFunction (_ : unit) (s : storage) : result =
  [], { s with value = call s.fn s.value }