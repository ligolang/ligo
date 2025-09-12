type t2 = A of int | B of int

module MyModule = struct
  type t5 = A of int | C of bool
  type t4 = A of int | D of int

  module MySubModule = struct
    type t6 = A of int | E of tez
  end
end

module MySecondModule = struct
  type t3 = A of int | F of int
end

type t1 = A of int | G of tez

// The compiler will search above for sum types with an 'A' constructor
let x = A 42