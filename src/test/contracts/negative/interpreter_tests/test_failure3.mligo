module C = struct [@entry] let f = (fun () () -> ()) end
let test =
  Test.originate (contract_of C) () 0tez
