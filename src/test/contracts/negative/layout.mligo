type storage = [@layout comb] {
  tata : int;
  toto : int;
  titi : int;
  tutu : int;
}

type storage1 = [@layout tree] {
  tata : int;
  toto : int;
  titi : int;
  tutu : int;
}
type return = operation list * storage

let main ((p,s) : unit * storage) : return =
  let s : storage1 = {
    tata = 0;
    toto = 1;
    titi = 2;
    tutu = 3;
  } in
  [],s
