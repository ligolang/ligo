type storage = int;

type param =
  | Add(int)
  | Sub(int);

let main2 = ((p: param), s: storage) => {
  let storage =
    s
    + (
      switch (p) {
      | Add(n) => n
      | Sub(n) => 0 - n
      }
  );
  (([]: list(operation)), storage);
};

let main = (x: (param, storage)) => main2(x[0],x[1]);
