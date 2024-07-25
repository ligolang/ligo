module Test = Test.Next

module C = struct
  [@entry]
  let rec main (p : unit) (s : unit) : operation list * unit =
    main p s
end

let test =
  let {taddr; code = _; size = _} = Test.Originate.contract (contract_of C) () 0tez in
  let _contr = Test.Typed_address.to_contract taddr in
  ()
