let dummy_check : unit = ()

[@entry]
let main () (s : int) : operation list * int = begin
  let res = s + 42 in
  dummy_check;
  dummy_check;
  (([] : operation list), s)
  end
