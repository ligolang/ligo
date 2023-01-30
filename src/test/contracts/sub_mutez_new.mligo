let sub (store : tez) (delta : tez) : tez option = store - delta

let main (_ : unit) (store : tez) : operation list * tez =
  ([] : operation list), Option.unopt (sub store 1tez)