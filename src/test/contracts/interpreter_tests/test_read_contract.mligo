let test_foo =
  let c = Test.read_contract_from_file "contract_under_test/compiled.tz" in
  let a = Test.originate_contract c (Test.eval ()) 0tez in
  Test.log a
