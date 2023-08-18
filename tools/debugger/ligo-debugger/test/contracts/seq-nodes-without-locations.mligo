let dummy_check : unit = ()

[@entry]
let main (_, s : unit * int) : operation list * int = begin
  let res = s + 42 in
  dummy_check;
  dummy_check;
  (([] : operation list), s)
  end
