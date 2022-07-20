let not_main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let s1 = s + 42;
  (([] : list (operation)), s1)
};
