let rec recursive = ((n, acc) : (int, int)) : int =>
  if (n == 0) { acc; } else { recursive((n - 1, acc + 2)); };

let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  (([] : list (operation)), recursive((s, 0)))
};
