type storage = (int, int);

let main = (n : (int, storage)) : (list (operation), storage) => {
  let x : (int, int) = {
    let x : int = 7;
    (x + n[0], n[1][0] + n[1][1]);
  };
  ([]: list (operation), x);
};
