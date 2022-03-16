let sub (store, delta : tez * tez) : tez option = store - delta

let main (_, store : unit * tez) : operation list * tez =
  ([] : operation list), Option.unopt (sub (store, 1tez))