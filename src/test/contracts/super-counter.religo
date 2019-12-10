type action =
  | Increment(int)
  | Decrement(int);

let main = (p: action, s: int): (list(operation), int) => {
  let storage =
    switch (p) {
    | Increment(n) => s + n
    | Decrement(n) => s - n
    };
  ([]: list(operation), storage);
};
