type storage =
  {
    storage : int;
    dynamic_entrypoints;
    extra : int
  }

[@entry]
  let foo () (_ : storage) : operation list * storage = failwith ()