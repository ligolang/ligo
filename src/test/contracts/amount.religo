let check_ = (_ : unit) : int =>
  if (Tezos.get_amount () == 100tez) { 42; } else { 0; };
