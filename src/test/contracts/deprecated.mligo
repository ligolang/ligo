[@deprecated "Replace me by...@.g!@.mail: foo@@bar.com"] let f () = 1
let g () = 2

module C = struct
  [@entry] let foo (() : unit) (m : int) : operation list * int = [], m + f ()
end

let test = Test.log (f ())
