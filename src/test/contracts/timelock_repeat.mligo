type storage = {
  last_use: timestamp;
  interval: int;
  execute: unit -> operation list;
}

let main (p,s: unit * storage) : operation list * storage =
  if Current.time > (s.last_use + s.interval)
  then
    let s: storage = {
                      last_use = Current.time;
                      interval = s.interval;
                      execute = s.execute;
                      }
    in
    (s.execute (), s)
  else
    (* TODO: Add the time until next use to this message *)
    (failwith "You have to wait before you can execute this contract again.":
       operation list * storage)
