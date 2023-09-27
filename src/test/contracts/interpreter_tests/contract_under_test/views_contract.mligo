type 'a return = operation list * 'a

module Main_with_view = struct
  [@view]
  let sto_plus_n (n : int) (s : int) : int = s + n + 1

  [@entry]
  let main (() : unit) (s : int) : int return =
    ([] : operation list), s
end

module Caller = struct
  [@entry]
  let main (addr : address) (_s : int) : int return =
    let x : int option = Tezos.call_view "sto_plus_n" 1 addr in
    let ret =
      match x with
        None -> (failwith "view call failed" : int)
      | Some i -> i in
    ([] : operation list), ret
end
