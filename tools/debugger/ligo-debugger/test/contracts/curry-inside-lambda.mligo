[@entry]
let main (_, s : unit * int) : operation list * int =
  let lambdaFun =
    fun (a, b : int * int) ->
      let apply (f : int -> int -> int) (a : int) (b : int) = f a b in
      let add (a : int) (b : int) = a + b in
      let sub (a : int) (b : int) = a - b in
      apply add a b * apply sub a b
  in (([] : operation list), lambdaFun(s, s + 2))
