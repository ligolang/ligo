module Test = Test.Next

module C = struct
  let michelson_add : int * int -> int =
    [%Michelson ({| { UNPAIR ; ADD } |} : int * int -> int) ]

  [@entry]
  let main (x : int) (s : int) : operation list * int =
    ([] : operation list), michelson_add (x, s)
end

let test =
  let orig = Test.Originate.contract (contract_of C) 1 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main 41) 0tez in
  Test.IO.log (Test.Typed_address.get_storage orig.taddr)
