module Test = Test.Next

#import "../../c/d/foo.mligo" "Foo"

let test_originate_from_file_relative_path : (unit, unit) typed_address =
  let x = Test.Originate.from_file
      "../../../src/contract/unit.mligo" () 0tez
  in x.taddr

let test_originate_from_file_relative_path_w_r_t_imported_file =
  let addr = Foo.originate () in
  let bef  = Test.Typed_address.get_balance addr in
  let ()   = ignore (Test.Typed_address.transfer addr () 10tez) in
  let aft  = Test.Typed_address.get_balance addr in
  aft = (bef + 10tez)
