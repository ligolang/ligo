let addOption = ((a, b) : (option(int), option(int))) : option(int) =>
  switch a {
  | Some x =>
      switch b {
      | Some y => Some (x + y)
      | None => a
      };
  | None => b
  };

let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let s1 = s + s;
  let s1 = Some (s1 * 2);
  let s2 = Some (s + 28);
  let s2 = addOption(s1, s2);
  let s2 = addOption(s2, s2);
  let res =
    switch s2 {
    | Some s => s + 10
    | None => 42
    };
  (([] : list(operation)), res)
}
