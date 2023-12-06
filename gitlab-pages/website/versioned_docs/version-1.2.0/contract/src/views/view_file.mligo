module C = struct
  type storage = string

  [@entry] let append (a : string) (s : storage) : operation list * storage = [] , s ^ a

  [@entry] let clear (_ : unit) (_ : storage) : operation list * storage = [] , ""

  let v (expected_length: nat) (s: storage) : bool = (String.length s = expected_length)
end