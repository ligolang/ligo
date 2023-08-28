module A = struct
  let current_turn = fun (i : nat) -> i + 1n

  let other =
    fun (n : nat) -> let current_turn = current_turn 1n in
      assert (n = current_turn)

end

[@entry]
let main (_p : unit) (_s : unit) : operation list * unit =
  ([] : operation list), A.other 2n
