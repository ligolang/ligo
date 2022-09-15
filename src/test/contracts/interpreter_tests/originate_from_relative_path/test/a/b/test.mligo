#import "../../c/d/foo.mligo" "Foo"

let test_originate_from_file_relative_path =
  let (c_addr,_,_) = Test.originate_from_file 
    "../../../src/contract/unit.mligo" 
    "main"
    ([] : string list)
    (Test.eval ()) 0tez in
  c_addr


let test_originate_from_file_relative_path_w_r_t_imported_file =
  Foo.originate ()