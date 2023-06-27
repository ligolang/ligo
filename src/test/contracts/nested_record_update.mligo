type level1 = {
  foo : int;
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

let update (s , n: level3 * int) : level3 =
  { s with
    buz = n;
    l2.fiz = n;
    l2.l1.foo = n;
    l2.l1.bar = n;
  }

let test () : level3 =
  let s : level3 = {
    buz = 0;
    l2 = { 
      fiz = 0;
      l1 = {
        foo = 0;
        bar = 0;
        buz = 0;
      }
    }
  } in
  let s1 = update (s, 5) in
  let () = assert (s1.buz = 5) in
  let () = assert (s1.l2.fiz = 5) in
  let () = assert (s1.l2.l1.bar = 5) in
  let () = assert (s1.l2.l1.foo = 5) in
  s1
