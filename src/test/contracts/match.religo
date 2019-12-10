type storage = int;

type param =
  | Add(int)
  | Sub(int);

let main = ((p: param), storage) => {
  let storage =
    storage
    + (
      switch (p) {
      | Add(n) => n
      | Sub(n) => 0 - n
      }
  );
  (([]: list(operation)), storage);
};
