let main = ((_, s) : (unit, option(int))) : (list(operation), option(int)) => {
  let s2 = switch s {
  | Some x => x + 1
  | None => 0
  };
  (([] : list (operation)), Some s2)
};
