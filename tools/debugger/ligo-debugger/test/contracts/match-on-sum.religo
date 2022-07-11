type variants =
  | Variant1 (int)
  | Variant2;

let main = ((p, s) : (variants, int)) : (list(operation), int) => {
  let s2 = switch p {
  | Variant1 (x) => s + x
  | Variant2 => s
  };
  (([] : list (operation)), s2)
};
