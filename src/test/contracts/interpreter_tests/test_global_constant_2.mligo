module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let ct : michelson_program = Test.constant_to_michelson_program "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
  let ct : string = Test.register_constant ct
  [@entry]
  let main (() : parameter)  (store : storage) : return =
  ([] : operation list), ((Tezos.constant ct : int -> int) store)
end

let test =
  let orig = Test.originate (contract_of C) 1 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  assert (Test.get_storage orig.addr = 5)
