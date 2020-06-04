type storage = tez;

let main2 = 
  (p: unit, s: storage) => 
    ([] : list(operation), Tezos.balance);

let main = (x: (unit, storage)) => main2(x[0], x[1]);
