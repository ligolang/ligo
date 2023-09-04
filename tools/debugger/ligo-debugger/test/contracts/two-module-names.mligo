let add (a, b : int * int) : int = a + b

let sub (a, b : int * int) : int = a - b

module Main1 = struct
  [@entry]
  let main1 () (s : int) : operation list * int =
    (([] : operation list), add(s, s + 2))
end

module Main2 = struct
  [@entry]
  let main2 (p : int) (s : int) : operation list * int =
    (([] : operation list), sub(p, s))
end

