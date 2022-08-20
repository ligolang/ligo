let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let s2 = s + 42;
  let s3 = s2 * s2 * 2;
  (([] : list (operation)), s2)
};
