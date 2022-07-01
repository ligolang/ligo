let main = ((p, s) : (int, list(int))) : (list(operation), list(int)) => {
  let s2 = switch s {
  | [] => [p]
  | [x, ...l] => [x, ...[p, ...[x, ...l]]]
  };
  (([] : list (operation)), s2)
};
