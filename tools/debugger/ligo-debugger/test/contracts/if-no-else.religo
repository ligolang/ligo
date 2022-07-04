let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let _ = if (s > 10) {failwith("no branches????")};
  (([] : list (operation)), s)
};
