// alias
module Test = Test.Next

// structure / contract
module C = struct
  type storage = unit

  // entrypoint attr
  [@entry] let f () () : operation list * unit = [], ()
end

// type: sum
type sum_type = 
  | Sum_of_int of int
  | Sum_of_string of string
  | Sum_of_enum

// function
let sum_test (input : sum_type) : int =
  match input with
  | Sum_of_int i -> i
  | Sum_of_string s -> String.size s |> int
  | Sum_of_enum -> 0

// type: record
type record_type =
  { recorda : int
  ; recordb : unit
  }

// value
let record_test = { recorda = 1; recordb = () }

// test value
let test =
  let orig = Test.originate (contract_of C) () 0tez in
  let _c = Test.to_contract orig.taddr in
  ()

