module MyContract = struct
  type storage = int
  type result = operation list * storage

  [@entry] let increment (delta : int) (storage : storage) : result =
    if abs delta <= 5n then [], storage + delta else failwith "Pass 5 or less"
  [@entry] let decrement (delta : int) (storage : storage) : result =
    if abs delta <= 5n then [], storage - delta else failwith "Pass 5 or less"
  [@entry] let reset () (_storage : storage) : result = [], 0
end
let test_failure =
  let initial_storage = 10 in
  let orig = Test.Next.Originate.contract (contract_of MyContract) initial_storage 0tez in
  let result: test_exec_result = Test.Next.Contract.transfer (Test.Next.Typed_address.get_entrypoint "increment" orig.taddr) 50 0tez in
  match result with
    Fail _x -> Test.Next.IO.log "Failed as expected"
  | Success _s -> failwith "This should not succeed"