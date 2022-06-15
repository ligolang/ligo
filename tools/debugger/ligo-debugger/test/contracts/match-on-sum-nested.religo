type variants =
  | Variant1 (int)
  | Variant2
  | Variant3 (option(int));

let main = ((p, s) : (variants, int)) : (list(operation), int) => {
  let s2 = switch p {
  | Variant1 (x) => s + x
  | Variant2 => 0
  | Variant3 p2 => switch p2 {
    | Some x => s + 3 * x
    | None => s
    }
  };
  (([] : list (operation)), s2)
};
