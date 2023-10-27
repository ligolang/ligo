type storage =
  { data : int
  ; metadata : unit }

[@entry]
let e (_ : unit) (s : storage) = ([] : operation list), s
