let test_foo =
  let c : (unit, unit) michelson_contract =
    Test.read_contract_from_file "contract_under_test/compiled.tz" in
  let s = Test.decompile (Test.parse_michelson "Unit") in
  let a = Test.originate_contract c s 0tez in
  Test.log a

let test_bar =
  let c : (unit, (int,string) map) michelson_contract = Test.read_contract_from_file "contract_under_test/other_compiled.tz" in
  let s = Test.decompile (Test.constant_to_michelson_program "{ Elt 1 \"hi\" }") in
  let a = Test.originate_contract c s 0tez in
  let s = Test.get_storage a in
  let () = Test.log (s) in
  ()
