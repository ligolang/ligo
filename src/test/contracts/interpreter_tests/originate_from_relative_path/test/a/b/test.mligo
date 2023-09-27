#import "../../c/d/foo.mligo" "Foo"

let test_originate_from_file_relative_path : (unit, unit) typed_address =
  let x = Test.originate_from_file "../../../src/contract/unit.mligo" () 0tez in
  x.addr


let test_originate_from_file_relative_path_w_r_t_imported_file =
  let addr = Foo.originate () in
  let bef  = Test.get_balance addr in
  let ()   = ignore (Test.transfer addr () 10tez) in
  let aft  = Test.get_balance addr in
  aft = (bef + 10tez)