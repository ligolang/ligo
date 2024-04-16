let test =
  let addr = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
  let taddr : (unit, unit) typed_address = Test.cast_address addr in
  let contract : (unit) contract = Test.to_contract taddr in
  contract