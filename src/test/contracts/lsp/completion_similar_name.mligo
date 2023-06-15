type action = Add of int | Sub of int | Reset

type storage = { number : int; previous_action : action }

module Math = struct
  let add (a : int) (b : int) : storage =
    { number = a + b; previous_action = Add b }

  let sub (a : int) (b : int) : storage =
    { number = a - b; previous_action = Sub b }

  let zero = { number = 0; previous_action = Reset }
end

let x = Math.

let y = Ma
