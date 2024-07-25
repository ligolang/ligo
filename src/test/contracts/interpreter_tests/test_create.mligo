module Test = Test.Next

module Factory = struct
  [@entry]
  let main (i : int) (s : address list) : operation list * address list =
    let main (d : int) (k : int) : operation list * int = ([], d + k) in
    let (op, addr) = Tezos.create_contract main (None : key_hash option) 1tz i in
    [op], addr :: s
end

let test =
  let orig = Test.Originate.contract (contract_of Factory) ([] : address list) 10tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main 42) 0tez in
  let addr : address =
    Option.value_with_error
      "option is None"
      (List.head (Test.Typed_address.get_storage orig.taddr)) in
  let taddr : (int, int) typed_address =
    Test.Address.to_typed_address addr in
  Test.IO.log (Test.Typed_address.get_storage taddr)
