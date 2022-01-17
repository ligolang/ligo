type recordi = { a : option(list(int)) , b : list(int) };

let t13 = (x:recordi) =>
  switch(x) {
  | { a : Some ([])          , b : [hd, ...tl] } => hd
  | { a : Some ([hd, ...tl]) , b : [] }          => hd
  }