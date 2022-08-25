let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let s2 = s + 42;
  (([] : list (operation)), s2)
};
