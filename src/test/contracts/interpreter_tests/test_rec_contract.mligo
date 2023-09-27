module C = struct
  [@entry]
  let rec main (p : unit) (s : unit) : operation list * unit =
    main p s
end

let test =
  let {addr = taddr; code = _; size = _} = Test.originate (contract_of C) () 0tez in
  let _contr = Test.to_contract taddr in
  ()
