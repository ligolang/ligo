module Factory = struct
  [@entry]
  let main (i : int) (s : address list) : operation list * address list =
    let main (d : int) (k : int) : operation list * int = ([], d + k) in
    let (op, addr) = Tezos.create_contract main (None : key_hash option) 1tz i in
    [op], addr :: s
end

let test =
  let orig = Test.originate (contract_of Factory) ([] : address list) 10tez in
  let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
  let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
  let taddr : (int, int) typed_address = Test.cast_address addr in
  Test.log (Test.get_storage taddr)
