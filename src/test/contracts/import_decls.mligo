let v1 = E1.v1
let v2: E2.t2 = v1

module I1 : E3.M = struct
  type t3 = int

  module I2 = struct
    type t4 = E4.t5
  end
end

let v3: I1.t3 = v1

module type I3 = sig
    val v6: E5.t5

    type t4 = int
end

module I4 : I3 = struct
    type t10 = int
end

module I5 : I6 (* not possible to be external here *) = struct
    type t5 = int
end

let v4 = fun x -> E6.y + x
let rec v6 (x : E7.t7) : E8.t8 = E9.y - x

include E10

let v7 = E11.v11

let v8 = Directory.E12.v12

let v9 : Directory_2.Directory_3.E13.t13 = Super__.E13.v14
