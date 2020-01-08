
type storage = int;

let main2 = (p: int, storage): string => ([]: list(operation), p + storage);

let main = (x: (int, storage)) : string => main2(x[0],x[1]);

