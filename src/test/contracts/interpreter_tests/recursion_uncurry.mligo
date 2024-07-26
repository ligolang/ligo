module Test = Test.Next

module C = struct
  let rec foo (n : int) (x : string) : string = if n = 0 then x else foo (n - 1) (String.concat x "toto")
  [@entry]
  let main (n : int) (s : string) : operation list * string =
    ([] : operation list), foo n s
end

let test =
  let orig = Test.Originate.contract (contract_of C) "" 1tez in
  orig.size
