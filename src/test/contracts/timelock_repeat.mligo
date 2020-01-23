type storage = {
  next_use: timestamp;
  interval: int;
  execute: unit -> operation list;
}

let main (p,s: unit * storage) : operation list * storage =
  (* Multiple calls to Current.time give different values *)
  let now: timestamp = Current.time in
  if now > s.next_use
  then
    let s: storage = {
                      next_use = now + s.interval;
                      interval = s.interval;
                      execute = s.execute;
                      }
    in
    (s.execute (), s)
  else
    (* TODO: Add the time until next use to this message *)
    (failwith "You have to wait before you can execute this contract again.":
       operation list * storage)
