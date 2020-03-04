
type storage = int;

let main2 = (p: int, s: storage): string => ([]: list(operation), p + s);

let main = (x: (int, storage)) : string => main2(x[0],x[1]);
