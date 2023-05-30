module C = struct
  module type I = sig
    val x : int
  end
end

(* we access C.I which is a path *)
module Foo : C.I = struct
  let x = 42
end

(* it also works inside another struct *)
module Bar = struct
  module Foo : C.I = struct
    let x = 42
  end
end
