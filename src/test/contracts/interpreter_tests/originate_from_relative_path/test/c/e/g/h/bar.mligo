let f = "../../../../../src/contract/unit.mligo"

let originate () =
    let (c_addr,_,_) = Test.originate_from_file  f "main" (Test.eval ()) 0tez in
    c_addr