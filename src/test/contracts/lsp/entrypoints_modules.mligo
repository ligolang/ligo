module Good_1 = struct
  [@entry]
  let ep_int : unit -> int -> operation list * int = fun () s -> [], s

end

module Good_2 = struct
  [@entry]
  let ep_int : unit -> int -> operation list * int = fun () s -> [], s

end

module Good_3 = struct
  [@entry]
  let ep_string : unit -> string -> operation list * string = fun () s -> [], s

  [@view]
  let x : unit -> string -> int = fun _ _ -> 3

end

module Bad_1 = struct
  [@entry]
  let ep_string : unit -> string -> operation list * string = fun () s -> [], s

  [@entry]
  let ep_int : unit -> int -> operation list * int = fun () s -> [], s

end

module Bad_2 = struct
  [@entry]
  let ep_string : unit -> string -> operation list * string = fun () s -> [], s

  [@entry]
  let ep_string : unit -> string -> operation list * string = fun () s -> [], s

end

module Bad_3 = struct
  [@entry]
  let bad_ep : unit -> operation list * string = fun () -> [], ""

end


module Bad_4 = struct
  [@entry]
  let ep_string : unit -> string -> operation list * string = fun () s -> [], s

  [@view]
  let x : string = "Hi"

end
