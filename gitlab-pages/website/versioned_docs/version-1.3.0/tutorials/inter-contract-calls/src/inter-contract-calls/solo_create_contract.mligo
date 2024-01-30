let op = Tezos.create_contract
  (fun (p : int) (s : int) -> [], p + s)
  None
  0mutez
  1