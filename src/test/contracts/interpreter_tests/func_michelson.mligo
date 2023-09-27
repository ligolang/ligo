module C = struct
  let michelson_add : int * int -> int =
    [%Michelson ({| { UNPAIR ; ADD } |} : int * int -> int) ]

  [@entry]
  let main (x : int) (s : int) : operation list * int =
    ([] : operation list), michelson_add (x, s)
end

let test =
  let orig = Test.originate (contract_of C) 1 0tez in
  let _ = Test.transfer_exn orig.addr (Main 41) 0tez in
  Test.log (Test.get_storage orig.addr)
