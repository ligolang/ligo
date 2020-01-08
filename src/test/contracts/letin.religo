type storage = (int, int);

let main2 = ((n : int), storage) => {
  let x: (int, int) = {
    let x: int = 7;
    (x + n, storage[0] + storage[1]);
  };
  ([]: list(operation), x);
};

let main = (x: (int, storage)) => main2(x[0],x[1]);
