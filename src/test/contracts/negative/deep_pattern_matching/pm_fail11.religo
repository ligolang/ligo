let t12 = (x : list(int)) =>
  switch(x) {
  | [hd, ...[hd2, ...tl]] => hd + hd2
  | [] => 0
  }