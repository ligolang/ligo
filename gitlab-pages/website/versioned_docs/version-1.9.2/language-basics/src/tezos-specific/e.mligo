let origination : operation * address = Tezos.create_contract
  (fun (p : nat) (s : string) -> ([], s))
  None
  3tz
  "initial_storage"