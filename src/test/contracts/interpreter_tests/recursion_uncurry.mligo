let rec foo (n : int) (x : string) : string = if n = 0 then x else foo (n - 1) (String.concat x "toto")

let main (n : int) (s : string) : operation list * string =
  ([] : operation list), foo n s

let test =
  let (_, _, n) = Test.originate main "" 1tez in
  n
