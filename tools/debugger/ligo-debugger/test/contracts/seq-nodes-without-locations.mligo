let dummy_check : unit = ()

let main (_, s : unit * int) : operation list * int = begin
  let res = s + 42 in
  dummy_check;
  dummy_check;
  (([] : operation list), s)
  end
