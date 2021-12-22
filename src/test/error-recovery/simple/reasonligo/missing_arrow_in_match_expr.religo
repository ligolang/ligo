type storage = int;

type parameter = list (int);

type s = {
  a : int,
  b : bool,
  c : string,
  d : list (list (list (int)))
};

let s_a = { a : 42, b : false };

let s_b = { ...s_a, a : 32 };

type return = (list (operation), storage);

let hd = (x : list (int)) : int =>
  switch x {
    | [] -1
    | [x, ...xs] => x
  };

let main = (a, b : (parameter, storage)) : return =>
  ([] : list (operation), hd (a) + (b + 8) * 11);
