type storage =
  {
    storage : int;
    dynamic_entrypoints;
  }

let two () () : operation list * unit = let _ = 1 in [%external ("OPT_OUT_ENTRY")]

[@entry]
let nope () (_ : storage) : operation list * storage = failwith ()
