module type I = sig
  type t
  type s = int
  val f : t -> s
end

module A : I = struct
  type t = int
  type s = int
  let f (x : t) : s = x
end

module B = struct
  type t = nat
  type s = int
  let f (x : t) : s = x + 0
end

module BI : I = B
