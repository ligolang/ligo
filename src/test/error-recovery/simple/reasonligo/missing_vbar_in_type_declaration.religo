type storage = int;

type parameter = list (int);

type return = L (list (operation)) S (storage);

let hd = (x : list (int)) : int =>
  switch x {
    | [] => -1
    | [x, ...xs] => x
  };

let main = (a, b : (parameter, storage)) : return =>
  L (hd (a) + (b + 8) * 11);
