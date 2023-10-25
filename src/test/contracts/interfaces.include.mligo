module type FA0 = sig
  type t
  [@entry] val transfer : unit -> t -> operation list * t
end

module type FA0EXT = sig
  include FA0
  [@entry] val transfer2 : unit -> t -> operation list * t
end

module FA0Impl : FA0 = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end

module FA0EXTImpl : FA0EXT = struct
  include FA0Impl
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end
