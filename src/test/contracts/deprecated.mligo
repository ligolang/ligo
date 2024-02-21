[@deprecated "Replace me by...@.g!@.mail: foo@@bar.com"] let f () = 1
let g () = 2

module C = struct
  [@entry] let foo (() : unit) (m : int) : operation list * int = [], m + f ()
end

let test = Test.log (f ())

[@deprecated "@!FOO!this is h, but only h or i will trigger"] let h () = 3
[@deprecated "@!FOO!this is h, but only h or i will trigger"] let i () = 3

let test2 = Test.log (h () + i ())
