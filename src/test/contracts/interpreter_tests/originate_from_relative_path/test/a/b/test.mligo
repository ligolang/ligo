#import "../../c/d/foo.mligo" "Foo"

let test_originate_from_file_relative_path =
  let (c_addr,_,_) = Test.originate_from_file 
    "../../../src/contract/unit.mligo" 
    (Test.eval ()) 0tez in
  c_addr


let test_originate_from_file_relative_path_w_r_t_imported_file =
  let addr = Foo.originate () in
  let bef  = Test.get_balance addr in
  let ()   = ignore (Test.transfer addr (Test.eval ()) 10tez) in
  let aft  = Test.get_balance addr in
  aft = (bef + 10tez)