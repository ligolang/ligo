type level1 = {
  foo : int ticket;
  bar : int;
  buz : int;
}

type level2 = {
  l1 : level1;
  fiz : int;
}

type level3 = {
  l2 : level2;
  buz : int;
}

let update (s , n, t: level3 * int * int ticket) : level3 =
  { s with
    buz = n;
    l2.fiz = n;
    l2.l1.foo = t;
    l2.l1.bar = n;
  }

let main ((n, t) : int * int ticket) (s : level3) : operation list * level3 =
  [], update (s, n, t)