module C = struct
  [@entry]
  let main (b : bytes) (_ : bytes) : operation list * bytes =
    ([] : operation list), b
end

let test =
  let b = Bytes.pack 42n in
  let orig = Test.originate (contract_of C) b 0tez in
  let () = assert ((Bytes.unpack (Test.get_storage orig.addr) : nat option) = Some 42n) in
  let b = Bytes.pack "bonjour" in
  let _ = Test.transfer_exn orig.addr (Main b) 0tez in
  assert ((Bytes.unpack (Test.get_storage orig.addr) : string option) = Some "bonjour")
