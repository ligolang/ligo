type parameter = unit

type storage = {
  next_use : timestamp;
  interval : int;
  execute  : unit -> operation list
}

type return = operation list * storage

let main (_ : parameter) (store : storage) : return =
  (* Multiple evaluations of Tezos.get_now () give different values *)
  let my_now : timestamp = Tezos.get_now () in
  if my_now > store.next_use
  then
    let next_use = my_now + store.interval in
    let store : storage =
      {store with next_use}
    in store.execute (), store
  else
    (* TODO: Add the time until next use to this message *)
    (failwith "You have to wait before you can execute this contract again."
     : return)
