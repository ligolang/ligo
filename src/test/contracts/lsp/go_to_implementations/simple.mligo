module type I = sig
  type t
end

module M : I = struct
  type t = int
end
