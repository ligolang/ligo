module A = struct
  let foo = 42
  let bar = 24
end

module type T = sig
  type t
  type int = string
end

module B : T = struct
  type t = nat
  type int = string

  let b : t = 1n
end

module C : sig
  val foo : tez
  val another : int
end = struct
  let foo = 1tez
  let another = 100
end

module Outer = struct
  let outer_foo a b = a - b

  module Inner = struct
    let inner_foo a b = a + b
  end
end

let thing_with_bytes = Bytes.unpack

module Bytes = struct
  let overwritten = "yes"
end

let another_thing_with_bytes = Bytes.overwritten

let inner_func = Outer.Inner.inner_foo

module Mangled = struct
  let where =
  let v = 42
end

module Mangled_with_sig : T = struct
  let where =
  type t = string
  type int = string
end

module Mangled_with_inlined_sig : sig
  val foo : int
end = struct
  let where =
  let foo = 42
end

let module_in =
  module M = struct
    let v = 42
  end in M.v

module type I = sig
  val b : bool
end

module type With_included = sig
  include T
  include I

  val z : int
end

module With_included = struct
  include struct
    type t = int
    type int = string
  end

  include struct
    let b = false
  end
end
