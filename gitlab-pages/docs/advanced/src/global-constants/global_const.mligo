let helper ((s, x) : string * int) =
  String.length s + x * 3 + 2

[@entry]
let main (p : string) (s : int) : operation list * int =
  ([], helper (p, s))