(* This tests that we subvert the Micheline whitespace check *)
let f = [%Michelson ({|
{ DROP;
UNIT
} |} : unit -> unit)]

[@entry]
let main (_ : unit) (s : unit) : operation list * unit =
  (([] : operation list), f s)
