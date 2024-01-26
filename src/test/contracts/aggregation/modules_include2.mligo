module A1 = struct
  type t = int
end

module A2 = struct
  include A1
end

module A3 = struct
  include A2
end

module A4 = struct
  include A3
end

module A5 = struct
  include A4
end

module A6 = struct
  include A5
end

module A7 = struct
  include A6
end

module A8 = struct
  include A7
end

module A9 = struct
  include A8
end

module A10 = struct
  include A9
end

module A11 = struct
  include A10

  [@entry]
  let main () () : operation list * unit = [], ()
end
