type storage =
  [@layout comb]
  {
   token_id : nat;
   [@annot] ignore_annotation : address list
  }

[@entry]
let main () (s : storage) : operation list * storage = [], s
