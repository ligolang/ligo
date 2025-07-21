let test =
  let addr : address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  let taddr : (unit, unit) typed_address = Test.Address.to_typed_address addr in
  let contract : unit contract = Test.Typed_address.to_contract taddr in
  contract