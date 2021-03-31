// This is mockup_testme.mligo
type storage = string

type parameter =
  Append of string

type return = operation list * storage

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Append (s) -> store ^ s)