module EURO =
  struct
    let add (a, b : int * int) : int = a + b
    let main (p, s : int * int) : operation list * int =
      (([] : operation list), add(p, s))
  end

[@entry]
let main () (s : int) : operation list * int = EURO.main(s, s + 2)
