module type I = sig
  type t

  val x : t
end

module M : I = struct
  type t = int

  let x : t = 42
end

let test = M.x
