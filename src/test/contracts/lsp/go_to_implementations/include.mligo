module type S = sig
  type t
end

module M1 = struct
  type t = int
end

module M2 = struct
  type t = int
end

module M3 : S = struct
  include M1
  include M2
  type t = int
end

module M4 = struct
  include M3
end

type t = M4.t
