module type FA0 = sig
  type storage

  [@entry] val add : int -> storage -> operation list * storage
end
