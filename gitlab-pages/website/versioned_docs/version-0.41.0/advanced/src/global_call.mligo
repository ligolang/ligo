let main ((p, s) : string * int) : operation list * int =
  let g = (Tezos.constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" : (string * int) -> int) in
  (([] : operation list), (g)(p, s))
