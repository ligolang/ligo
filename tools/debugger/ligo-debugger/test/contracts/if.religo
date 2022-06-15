let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let s2 = if (s > 10) {s * 2} else {s / 2};
  (([] : list (operation)), s2)
};
