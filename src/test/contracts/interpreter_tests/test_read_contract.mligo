module Test = Test.Next

let test_foo =
  let c : (unit, unit) michelson_contract =
    Test.Michelson.Contract.from_file "contract_under_test/compiled.tz" in
  let s = Test.Michelson.decompile (Test.Michelson.parse "Unit") in
  let a = Test.Originate.michelson c s 0tez in
  Test.IO.log a

let test_bar =
  let c : (unit, (int,string) map) michelson_contract =
    Test.Michelson.Contract.from_file
      "contract_under_test/other_compiled.tz" in
  let s = Test.Michelson.decompile
            (Test.Michelson.parse "{ Elt 1 \"hi\" }") in
  let a = Test.Originate.michelson c s 0tez in
  let s = Test.Typed_address.get_storage a in
  let () = Test.IO.log (s) in
  ()
