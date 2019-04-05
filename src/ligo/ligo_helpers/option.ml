let unopt ~default = function
  | None -> default
  | Some x -> x
