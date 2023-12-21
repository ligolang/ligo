module type T = sig
  type t
end

module type I = sig
  val b : bool
end

module type I_with_included = sig
  include T
  include I

  val z : int
end

module type Alias = I_with_included

module With_included : I_with_included = struct
  include struct
    type t = int
  end

  let z = 0

  include struct
    let b = false
  end
end

module With_included2 : Alias = struct
  type t = int

  let b = false
  let z = 0
end
