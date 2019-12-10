type storage = (int, int);

let main = ((n : int), storage) => {
  let x: (int, int) = {
    let x: int = 7;
    (x + n, storage[0] + storage[1]);
  };
  ([]: list(operation), x);
};
