module First = struct
  [@entry] let main () (s : int) : operation list * int = [], s
end

module Second = struct
  [@entry] let main () (s : int) : operation list * int = [], s + 2
end
