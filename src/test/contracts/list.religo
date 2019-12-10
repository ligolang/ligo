type storage = (int, list(int));

type param = list(int);

let x: list(int) = [];
let y: list(int) = [3, 4, 5];
let z: list(int) = [2, ...y];

let main = (p: param, storage) => {
  let storage =
    switch (p) {
    | [] => storage
    | [hd, ...tl] => (storage[0] + hd, tl)
    };
  ([]: list(operation), storage);
};

let fold_op = (s: list(int)): int => {
  let aggregate = (prec: int, cur: int) => prec + cur;
  List.fold(aggregate, s, 10);
};

let map_op = (s: list(int)): list(int) =>
  List.map((cur: int) => cur + 1, s);

let iter_op = (s: list(int)): unit => {
  let do_nothing = (z: int) => unit;
  List.iter(do_nothing, s);
};
