let sub (store, delta : tez * tez) : tez = store - delta

let main (_, store : unit * tez) : operation list * tez =
  ([] : operation list), sub (store, 1tez)